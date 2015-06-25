import java.math.BigInteger;

public class Factorial
{
    // Evaluate n!
    public static BigInteger factorial( BigInteger n )
    {
        if( n.equals(BigInteger.ONE) )     // base case
            return BigInteger.ONE;
        else
            return n.multiply(factorial(n.subtract(BigInteger.ONE)));
    }

    // Simple test program
    public static void main( String [ ] args )
    {
        for( int i = 1; i <= 10; i++ )
            System.out.println( factorial( BigInteger.valueOf(i) ) );
    }
}