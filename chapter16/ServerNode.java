import com.ericsson.otp.erlang.*; 
import java.math.BigInteger;


public class ServerNode { 

   public static void main (String[] _args) throws Exception{ 

      OtpNode bar = new OtpNode("bar"); 
      OtpMbox mbox = bar.createMbox("facserver");

      OtpErlangObject o;
      OtpErlangTuple  msg;
      OtpErlangPid    from;
      BigInteger      n;
      
      OtpErlangAtom ok = new OtpErlangAtom("ok");
      

      while(true) try { 
	      o    = mbox.receive();
	      msg  = (OtpErlangTuple)o;
	      from = (OtpErlangPid)(msg.elementAt(0));
	      n    = ((OtpErlangLong)(msg.elementAt(1))).bigIntegerValue();
	      OtpErlangObject[] reply = new OtpErlangObject[2];
	      reply[0] = ok;
	      reply[1] = new OtpErlangLong(Factorial.factorial(n));
	      OtpErlangTuple tuple = new OtpErlangTuple(reply);
	      mbox.send(from,tuple); 

      }catch(OtpErlangExit e) { break; } 
   } 
} 

