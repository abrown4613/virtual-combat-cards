/*
 * Copyright (C) 2008-2013 - Thomas Santana <tms@exnebula.org>
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
package vcc.dnd4e.web.servlet

import javax.servlet.http.{HttpServletResponse, HttpServletRequest, HttpServlet}
import java.net.{Inet4Address, InetAddress}

class GreetingServlet extends HttpServlet {

  private def makePage(buttons: Seq[(String, String)], addresses: Seq[String]) = (<html>
    <head>
      <title>Virtual Combat Cards Web Interfaces</title>
      <link href="bootstrap/css/bootstrap.css" rel="stylesheet" media="screen"/>
      <link href="bootstrap/css/bootstrap-responsive.css" rel="stylesheet" media="screen"/>
      <meta name="viewport" content="width=device-width, initial-scale=1.0"/>
    </head>
    <body>
      <div class="container" ng-hide="running">
        <div class="row"></div>
        <div class="navbar">
          <div class="navbar-inner">
            <a class="brand" href="#">Virtual Combat Cards</a>
          </div>
        </div>
        <div class="row">
          <div class="span6">
            {buttons.map(button =>
            <a class="btn btn-primary btn-large btn-block" href={button._1}>
              {button._2}
            </a>
          )}
          </div>
          <div class="span6">
            <h4>Connection links</h4>
            <table class="table">
              {addresses.map {
              address =>
                <tr>
                  <td>
                    <h4>
                      <a href={address}>
                        {address}
                      </a>
                    </h4>
                  </td>
                </tr>
            }}
            </table>
          </div>
        </div>
      </div>
    </body>
  </html>)

  override def doGet(req: HttpServletRequest, resp: HttpServletResponse) {
    resp.setContentType("text/html")
    resp.getWriter.println("<!DOCTYPE html>")
    resp.getWriter.append(makePage(
      Seq(
        ("/player.html", "Player View"),
        ("/manual", "User Manual")),
      enumerateAddresses(req.getLocalPort)).toString())
  }

  private def enumerateAddresses(port: Int): Seq[String] = {
    val localhost = InetAddress.getLocalHost
    val addrs = InetAddress.getAllByName(localhost.getCanonicalHostName).toList
    for (addr <- addrs if (addr.isInstanceOf[Inet4Address])) yield
      "http://%s:%d/".format(addr.getHostAddress, port)
  }
}