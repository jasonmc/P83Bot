(*  Copyright (C) 2017 Jason McCandless

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License along
    with this program; if not, write to the Free Software Foundation, Inc.,
    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA. *)

module P83Bot.Mikkeller

open HtmlAgilityPack

let getMenu ()  = async {
    let! page = FSharp.Data.Http.AsyncRequestString "https://www.mikkellernyc.com/menus/"
    let doc = new HtmlDocument()
    doc.LoadHtml(page)
    return (doc.DocumentNode.SelectNodes("//section[@id='on-draft']//h3[contains(@class, 'menu-item__heading')]")
           |> Seq.map (fun x -> x.InnerText)
           |> String.concat "\n")
           .[0..999]
}
