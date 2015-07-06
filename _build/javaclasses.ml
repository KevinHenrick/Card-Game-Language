(* Author: Mark Micchelli *)

let string_of_card_class =
"
    public class Card {
    public Card(Integer value, String suit) {
        this.value = value;
        this.suit = suit;
    }

    public Card(int i, char s) {
        value = new Integer(i);
        suit = String.valueOf(s);
    }
    

    public Integer getValue() { return value; }
    public String getSuit() { return suit; }
    public boolean equals(Object other)
    {
        if (other instanceof Card)
        {
            Card that = (Card) other;
            boolean sameValue = false;
            boolean sameSuit = false;
            if (value.equals(that.getValue()) ||
                that.getValue().intValue() == 0)
                sameValue = true;
            if (suit.equals(that.getSuit()) || that.getSuit().equals(\"*\"))
                sameSuit = true;
            return sameValue && sameSuit;
        }
        return false;
    }

    private Integer value;
    private String suit;
    }
"

let string_of_list_class =
"
import java.util.*;

public class CGLList 
{
	public CGLList()
	{
		list = new LinkedList<Object>();
	}

	public CGLList(Object... elements)
	{
		list = new LinkedList<Object>();
		for (Object ele : elements)
			list.addLast(ele);
	}

	public void addFirst(Object ele)
	{
		list.addFirst(ele);
	}

	public void addLast(Object ele)
	{
		list.addLast(ele);
	}

    // the remove methods are the source of the casting warnings
	public <T> T removeFirst()
	{
		return (T) list.removeFirst();
	}

	public <T> T removeLast()
	{
		return (T) list.removeLast();
	}

	public int size()
	{
		return list.size();
	}

	public Object[] toArray()
	{
		return list.toArray();
	}

    public ListIterator<Object> listIterator(int index)
    {
        return list.listIterator(index);
    }

	public boolean equals(Object other)
	{
        if (other instanceof CGLList)
        {
            CGLList that = (CGLList) other;
		    if (this.size() != that.size()) return false;
            ListIterator<Object> i1 = this.listIterator(0);
            ListIterator<Object> i2 = that.listIterator(0);
            while (i1.hasNext())
            {
                if (!i1.next().equals(i2.next())) return false;
            }
            return true;
        }
        return false;
    }

    private LinkedList<Object> list;
}
"
