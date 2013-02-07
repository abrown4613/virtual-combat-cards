/*
 *  Copyright (C) 2008-2013 - Thomas Santana <tms@exnebula.org>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>
 */
package vcc.dndi.reader

import language.postfixOps

/**
 * Provide a way to filtering and rewriting of token in a stream.
 */
trait TokenStreamRewrite[T] {
  /**
   * This function takes one token and should return a list of Tokens. This allows rewriting a token to a sequence of
   * tokens. After the rewrite the <code>TokenStream</token> will consume these tokens in sequence WITHOUT rewriting.
   */
  def rewriteToken(token: T): List[T]
}

/**
 * Provides a convenient way to rewrite stream of token. It uses a partial function to rewrite a token. If the
 * partial function is applicable to the term it's rewrites. Otherwise it leaves the token untouched.
 * @param conditional Partial function that may rewrite the token, if it return Nil the token will be filtered out,
 * otherwise the list will prepend to the original list.
 */
class ConditionalTokenStreamRewrite[T](conditional: PartialFunction[T, List[T]]) extends TokenStreamRewrite[T] {
  /**
   * Rewrite term if conditional partial function is defined for that token.
   */
  def rewriteToken(token: T): List[T] = if (conditional.isDefinedAt(token)) conditional(token) else List(token)
}

/**
 * Utility class for recursive reader/builders. It encapsulates a lists of tokens and works as an iterator fashion.
 * Whenever an advances is issued the derived classes can add additional logic to include special tokens.
 * You MUST call advance prior to reading the head of the stream.
 * @param  tokens tokens to be rewritten
 * @param  rewrite Optional TokenStreamRewriter that will be evoked on each of the
 */
final class TokenStream[T](tokens: List[T], rewrite: TokenStreamRewrite[T]) {
  private var original = if (rewrite != null) tokens else Nil
  private var rewritten = if (rewrite != null) Nil else tokens
  private var _next: T = null.asInstanceOf[T]

  def this(tokens:List[T]) = this(tokens,null)

  /**
   * Return the current head of the of the stream. It will not change the information.
   */
  def head: T = {
    if (_next == null) throw new IllegalStateException("Must call advance prior to reading TokenStream head")
    _next
  }

  /**
   * Discard current head and advance, advancing will call implementation of <code>nextToken</code>
   * @return True if the advance worked, false if we reached the end of the stream.
   */
  def advance(): Boolean = {
    // If we have no on the rewritten list get the next rewritten
    if (rewritten.isEmpty && (rewrite != null)) {
      while (!original.isEmpty && rewritten.isEmpty) {
        rewritten = rewrite.rewriteToken(original.head)
        original = original.tail
      }
    }
    // Consume rewrites
    if (rewritten isEmpty) false
    else {
      _next = rewritten.head
      rewritten = rewritten.tail
      true
    }
  }

  /**
   * Returns elements that have not been processed yet.
   */
  def remainder() = original
}
