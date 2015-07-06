(* Author: Mark Micchelli *)

let teq =
    "
    private static boolean teq(Object o1, Object o2)
    {
        return o1.getClass().isAssignableFrom(o2.getClass());
    }
    "

let intToString =
    "
    private static String intToString(Object toCast)
    {
        Integer i = (Integer) toCast;
        return i.toString();
    }
    "

let doubleToString = 
    "
    private static String doubleToString(Object toCast)
    {
        Double d = (Double) toCast;
        return d.toString();
    }
    "


let stringToInt =
    "
    private static Integer stringToInt(Object toCast)
    {
        String s = (String) toCast;
        return Integer.parseInt(s);
    }
    "

let stringToDouble =
    "
    private static Double stringToDouble(Object toCast)
    { 
       String s = (String) toCast;
       return Double.parseDouble(s);
    }
    "


let scan =
    "
    private static String scan()
    {
        Scanner input = new Scanner(System.in);
        return input.nextLine();
    }
    "

let print =
    "
    private static void print(Object toCast)
    {
        String toPrint = (String) toCast;
        System.out.print(toPrint);
    }
    "

let value =
    "
    private static Integer value(Object toCast)
    {
        Card c = (Card) toCast;
        return c.getValue();
    }
    "

let suit =
    "
    private static String suit(Object toCast)
    {
        Card c = (Card) toCast;
        return c.getSuit();
    }
    "

let random =
    "
    private static Integer random(Object toCast1, Object toCast2)
    {
        Integer lower = (Integer) toCast1;
        Integer higher = (Integer) toCast2;
        Random r = new Random();
        return lower + r.nextInt(higher + 1);
    }
    "

(* The Fisher-Yates shuffle: en.wikipedia.org/wiki/Fisher-Yates_shuffle *)
let shuffle =
    "
    private static CGLList shuffle(Object toCast) 
    {
        CGLList l = (CGLList) toCast;
        Object[] a = l.toArray();
        int len = a.length;
        Random r = new Random();
        for (int i = len - 1; i >= 1; i--)
        {
            int j = r.nextInt(i + 1);
            Object temp = a[i];
            a[i] = a[j];
            a[j] = temp;
        }
        
        CGLList res = new CGLList();
        for (Object ele : a)
            res.addLast(ele);
        return res;
    }
    "

let nemo =
    "private static Player NEMO = Player.NEMO;\n"

let standard =
    "private static CGLList STANDARD;\n"

let fillConst =
    "STANDARD = new CGLList();
    STANDARD.addLast(new Card(2, 'C'));
    STANDARD.addLast(new Card(3, 'C'));
    STANDARD.addLast(new Card(4, 'C'));
    STANDARD.addLast(new Card(5, 'C'));
    STANDARD.addLast(new Card(6, 'C'));
    STANDARD.addLast(new Card(7, 'C'));
    STANDARD.addLast(new Card(8, 'C'));
    STANDARD.addLast(new Card(9, 'C'));
    STANDARD.addLast(new Card(10, 'C'));
    STANDARD.addLast(new Card(11, 'C'));
    STANDARD.addLast(new Card(12, 'C'));
    STANDARD.addLast(new Card(13, 'C'));
    STANDARD.addLast(new Card(14, 'C'));
    STANDARD.addLast(new Card(2, 'D'));
    STANDARD.addLast(new Card(3, 'D'));
    STANDARD.addLast(new Card(4, 'D'));
    STANDARD.addLast(new Card(5, 'D'));
    STANDARD.addLast(new Card(6, 'D'));
    STANDARD.addLast(new Card(7, 'D'));
    STANDARD.addLast(new Card(8, 'D'));
    STANDARD.addLast(new Card(9, 'D'));
    STANDARD.addLast(new Card(10, 'D'));
    STANDARD.addLast(new Card(11, 'D'));
    STANDARD.addLast(new Card(12, 'D'));
    STANDARD.addLast(new Card(13, 'D'));
    STANDARD.addLast(new Card(14, 'D'));
    STANDARD.addLast(new Card(2, 'H'));
    STANDARD.addLast(new Card(3, 'H'));
    STANDARD.addLast(new Card(4, 'H'));
    STANDARD.addLast(new Card(5, 'H'));
    STANDARD.addLast(new Card(6, 'H'));
    STANDARD.addLast(new Card(7, 'H'));
    STANDARD.addLast(new Card(8, 'H'));
    STANDARD.addLast(new Card(9, 'H'));
    STANDARD.addLast(new Card(10, 'H'));
    STANDARD.addLast(new Card(11, 'H'));
    STANDARD.addLast(new Card(12, 'H'));
    STANDARD.addLast(new Card(13, 'H'));
    STANDARD.addLast(new Card(14, 'H'));
    STANDARD.addLast(new Card(2, 'S'));
    STANDARD.addLast(new Card(3, 'S'));
    STANDARD.addLast(new Card(4, 'S'));
    STANDARD.addLast(new Card(5, 'S'));
    STANDARD.addLast(new Card(6, 'S'));
    STANDARD.addLast(new Card(7, 'S'));
    STANDARD.addLast(new Card(8, 'S'));
    STANDARD.addLast(new Card(9, 'S'));
    STANDARD.addLast(new Card(10, 'S'));
    STANDARD.addLast(new Card(11, 'S'));
    STANDARD.addLast(new Card(12, 'S'));
    STANDARD.addLast(new Card(13, 'S'));
    STANDARD.addLast(new Card(14, 'S'));"

let coreFunc =
    teq ^ intToString ^ stringToInt ^ doubleToString ^ stringToDouble ^ suit ^
    value ^ print ^ scan ^ random ^ shuffle 

let coreConst = nemo ^ standard 
