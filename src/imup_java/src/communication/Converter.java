package communication;

import java.io.IOException;
import com.ericsson.otp.erlang.*;
import java.lang.Thread;
import java.util.HashMap;
import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNode;

public class Converter {

    static String server = "server";
    Client client = new Client();
    HashMap<String, OtpMbox> mailboxes = new HashMap<String, OtpMbox>();

    public void startup_communication(String id) {
        OtpNode self = null;
        OtpMbox mbox = null;

        try {            
            self = new OtpNode(id, "test");
            mbox = self.createMbox("facserver");
            if (self.ping(server, 2000)) {
                mailboxes.put(id, mbox);               
            } else {
                System.out.println("remote is not up");
                return;
            }
        } catch (IOException e1) {
            e1.printStackTrace();
        }
    }

    public void send_message(String id, String spel, String message) {
        if (!mailboxes.containsKey(id + spel)) {        
            startup_communication(id + spel);
        }
                 
        OtpErlangObject[] msg = new OtpErlangObject[4];
        msg[0] = mailboxes.get(id + spel).self();
        msg[1] = new OtpErlangAtom(id);
        msg[2] = new OtpErlangAtom(spel);        
        msg[3] = new OtpErlangAtom(message);
        OtpErlangTuple tuple = new OtpErlangTuple(msg);
        mailboxes.get(id + spel).send("pong", server, tuple);                        
        receive_message(id + spel);
    }

    /**
     *
     * @param num
     * @param personId
     * @param spelid
     * @param message
     */
    public void send_message(int num, String personId, String spelid, String message) {
        if (!mailboxes.containsKey(personId + spelid)) {
            startup_communication(personId + spelid);
            send_message(personId, spelid, "ping");
        }
        OtpErlangObject[] id = new OtpErlangObject[2];
        id[0] = new OtpErlangAtom(personId);
        id[1] = new OtpErlangAtom(spelid);
        
        OtpErlangObject[] msg = new OtpErlangObject[4];
        msg[0] = mailboxes.get(personId + spelid).self();
        msg[1] = new OtpErlangInt(num);
        msg[2] = new OtpErlangTuple(id);
        msg[3] = new OtpErlangAtom(message);
        OtpErlangTuple tuple = new OtpErlangTuple(msg);
        mailboxes.get(personId + spelid).send("pong", server, tuple);
        
    }

    public String receive_message(String id) {
        String message = "";
        while (true) {
            try {                
                OtpErlangObject robj = mailboxes.get(id).receive();
                
                OtpErlangTuple rtuple = (OtpErlangTuple) robj;       
                OtpErlangPid fromPid = (OtpErlangPid) (rtuple.elementAt(0));
                OtpErlangObject rmsg = rtuple.elementAt(1);

                System.out.println("Message: " + rmsg + " received from:  "
                        + fromPid.toString());
                 message = rmsg.toString();           
                break;
               
            } catch (OtpErlangExit e) {
                e.printStackTrace();
                break;
            } catch (OtpErlangDecodeException e) {
                e.printStackTrace();
            }
        }
        return message;
    }
}
