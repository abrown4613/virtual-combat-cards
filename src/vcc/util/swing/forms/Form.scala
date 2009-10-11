//$Id$
/**
 * Copyright (C) 2008-2009 tms - Thomas Santana <tms@exnebula.org>
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
package vcc.util.swing.forms

import scala.swing._
import scala.swing.event._
import vcc.util.swing.MigPanel

import vcc.infra.fields._

class Form(val prefix:String) {
  private var fields:List[FormField[_]] = Nil
  private var changeAction: (FormField[_])=>Unit = null
  
  def registerField(ff:FormField[_]) {
    fields = fields ::: List(ff)
  }
  
  def layout(mp:MigPanel) {
    for(f<-fields) f.layout(mp)
  }
  
  def isValid:Boolean = fields.map(_.hasValidValue).foldLeft(true)(_ && _)
  
  def setChangeAction(f: (FormField[_])=>Unit) {
    changeAction = f
  }
  
  def notifyChange(formfield:FormField[_]) {
    changeAction(formfield)
  }
  
  def extractMap:Map[String,String] = Map(fields.map(f=> f.id -> f.storageString) :_*)

}

trait FormField[T] extends TextComponent {
  private val _errorLabel = new Label("")
  private val _header = new Label("")
  
  protected val form:Form
  protected val validator:FieldValidator[T]
  protected val field:TextComponent
  protected val key:String
  
  def id:String = if(form.prefix != null) form.prefix + key else key
  
  private var fvalue:FieldValue[T] = Undefined
  
  def hasValidValue = fvalue.isValid
  
  def storageString = fvalue.storageString
  
  _errorLabel.foreground = java.awt.Color.RED
  
  reactions += {
    case ValueChanged(t:TextComponent) =>
      fvalue = validator.validate(t.text)
      setFieldState(false)
      form.notifyChange(this)
  }

  
  def setup(header:String,initValue:FieldValue[T]) {
    _header.text = header
    form.registerField(this)
    fvalue = initValue
    setFieldState(true)
    listenTo(field)
  }
  
  protected def setFieldState(setText:Boolean) {
    if(setText) field.text = fvalue.storageString
    field.background = if(fvalue.isValid) java.awt.Color.WHITE else java.awt.Color.PINK
    _errorLabel.text =  if(fvalue.isValid) "" else fvalue.asInstanceOf[Invalid[T]].reason 
  }
  
  def layout(mp:MigPanel) {
    mp.add(_header,"gap rel")
    mp.add(field,"gap rel, width 33%")
    mp.add(_errorLabel,"width 33%,wrap")
  }
}

class FormTextField[T](header:String, val key:String, fv:FieldValue[T], val form:Form,val validator:FieldValidator[T]) extends TextField with FormField[T] {
  val field = new TextField()
  def this(label:String,f:Field[T],form:Form) {
    this(label,f.id,f.fieldValue,form,f.validator)
  }
  setup(header,fv)
}
