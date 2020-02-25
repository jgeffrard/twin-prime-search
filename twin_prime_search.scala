import scala.util.control._
import scala.collection.mutable.ArrayBuffer

object TwinPrime
{
 def main(args: Array[String]): Unit = {
  println("Please enter the number up to which you would like to see the prime numbers: ");
  val input = scala.io.StdIn.readInt();
  var line = 1;
  println();

  println(s"List of prime numbers from 1 through $input: ");

  for (a <- findPrimes(input) )
  {     
    print(s"$a ");

    if (line == 25)
    {
        print("\n");
        line = 1;
    }

    line = line + 1;
  }

  println();
  println();
  println(s"List of twin primes from 1 through $input: "); 

  for (a <- twinPrimeSearch(findPrimes(input) ) )
  {
      print(s"$a ");

      if (line == 10)
      {
        print("\n");
        line = 1;
      }

      line = line + 1;
  }
    
 }

 def isPrime(n : Int) : Boolean = { // let's say n was 4
    /*
    Proof: The definition of a prime number is a positive integer that has exactly two positive divisors. However, 1 only has one
    positive divisor (1 itself), so it is not prime.
    */
     for (i <- 1 to n)
     {
         if ( ( (n % i) == 0) && (i != n) && (i != 1) )
         {
             return false;
         }

         if (n == 1)
         {
             return false;
         }
     }

     return true;
 }

 def findPrimes(p : Int) : ArrayBuffer[Int] = {
     val primes = new ArrayBuffer[Int]()

     for (i <- 1 to p)
     {
         if ( isPrime( i ) )
         {
             primes += i
         }
     }

     return primes
 }

 def twinPrimeSearch(list : ArrayBuffer[Int]) : ArrayBuffer[(String, String)] = {
    val listTwins = new ArrayBuffer[(String, String)]()

    val loop = new Breaks;

    loop.breakable {
        for (i <- 0 to (list.length - 1) )
        {        
            if (i != (list.length - 1))
            {
                // Find the absolute value of difference of i and i + 1. By using the absolute value we do not have to
                // go back and check for i - 1.
                if ( ( list(i) - list(i+1) ).abs == 2 )
                {
                    val j = ( list(i).toString, list(i+1).toString )

                    listTwins += j
                }
            }
            else
            {
                loop.break;
            }
        }
    }    

    return listTwins;
 }
}