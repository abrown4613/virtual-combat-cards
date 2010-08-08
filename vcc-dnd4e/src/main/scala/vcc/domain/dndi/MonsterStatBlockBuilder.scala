/**
 * Copyright (C) 2008-2010 - Thomas Santana <tms@exnebula.org>
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
//$Id$
package vcc.domain.dndi

object MonsterStatBlockBuilder extends StatBlockBuilder {
  import StatBlockBuilder._

  private val headBlock = Para("flavor",
    Line(PairInitFmt("Initiative"), PairSpacedFmt("Senses")),
    Group("Auras", Line(BoldFormat("Name", true), TextFormat("description", true))),
    PairFlexFmt("HP", " %s; "), PairSpacedFmt("Bloodied"), Break,
    Line(PairSpacedFmt("Regeneration")),
    PairFlexFmt("AC", " %s; "), PairFlexFmt("Fortitude", " %s, "), PairFlexFmt("Reflex", " %s, "), PairSpacedFmt("Will"), Break,
    Line(PairFlexFmt("Immune", " %s; "), PairFlexFmt("Resist", " %s; "), PairFlexFmt("Vulnerable", " %s; ")),
    Line(PairInitFmt("Saving Throws")), Line(PairSpacedFmt("Speed")), Line(PairSpacedFmt("Action Points"))
    )
  private val tailBlock = Para("alt flavor",
    PairSpacedFmt("Alignment"), PairSpacedFmt("Languages"), Break,
    Line(PairSpacedFmt("Skills")),
    PairSpacedFmt("Str"), PairSpacedFmt("Dex"), PairSpacedFmt("Wis"), Break,
    PairSpacedFmt("Con"), PairSpacedFmt("Int"), PairSpacedFmt("Cha"), Break,
    Line(PairSpacedFmt("Equipment"))
    )

  private val powerBlock = Seq(
    Para("flavor alt", ImageMap("Type"), BoldFormat("Name", true), TextFormat("Action", true), IfDefined("Keywords", Image("x.gif"), BoldFormat("Keywords", true))),
    Para("flavorIndent", MultiLineFormat("Description")),
    Group("POWER DESCRIPTION SUPPLEMENT", Para("flavor", EmphasisFormat("header", false)), Para("flavorIndent", TextFormat("description", false)))
    )


  def generate(ds: StatBlockDataSource) = {
    val block = new ChunkGroup(headBlock, Group("Powers", powerBlock: _*), tailBlock)
    (<html>
      <head>
          <link rel="stylesheet" type="text/css" href="dndi.css"/>
      </head>
      <body>
        <div id="detail">
          <h1 class="monster">
            {ds.extract("NAME").get}<span class="type">
            {ds.extract("TYPE").get + " " + ds.extract("ROLE").get}
          </span>
          </h1>{block.render(ds)}
        </div>
      </body>
    </html>)
  }
}