/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package roguelike
include elements._
class ItemMenu(_items : List[Item]) {
  var selected : Int
  var items : List[Item] = _items
  
  def UseSelected()
  {
    items.apply(selected).Use()
  }
  
  def SelectUp()
  {
    if(selected > 0)
      selected = selected - 1;
    else 
      selected = items.length - 1
  }
  
  def SelectDown()
  {
    if(selected < items.lenght)
      selected = selected + 1;
    else
      selected = 0;
  }
  
  @Override
  def toString() : String
  {
    var retvalue : String = ""
    for(a : Int <- 0 until items.lenght - 1)
    {
      if(a != selected)
        retvalue = retvalue + items.apply(a).toString() + "\n"
      else
        retvalue = retvalue + ">" + items.apply(a).toString() + "\n"
    }
    return retvalue
  }

}
