package communication;

import java.io.IOException;
import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNode;
import java.util.logging.Level;
import java.util.logging.Logger;

public class Converter {
    static String server = "server";
    Client client = new Client();
    
    public OtpMbox createMailbox(String id) {
        OtpNode self = null;
        OtpMbox mbox = null;

        try {
            self = new OtpNode(id, "test");
            mbox = self.createMbox("facserver");
            if (self.ping(server, 2000)) {
                return mbox;         
            } else {
                System.out.println("remote is not up");
                return null;
            }
        } catch (IOException e1) {
            e1.printStackTrace();
        }
        return mbox;
    }

    public void send_messagePing(String id, String spel, OtpMbox mailbox) {
        OtpErlangObject[] msg = new OtpErlangObject[4];
        msg[0] = mailbox.self();
        msg[1] = new OtpErlangAtom(id);
        msg[2] = new OtpErlangAtom(spel);
        msg[3] = new OtpErlangAtom("ping");
        OtpErlangTuple tuple = new OtpErlangTuple(msg);
        mailbox.send("pong", server, tuple);
        try {
            mailbox.receive();
        } catch (OtpErlangExit ex) {
            Logger.getLogger(Converter.class.getName()).log(Level.SEVERE, null, ex);
        } catch (OtpErlangDecodeException ex) {
            Logger.getLogger(Converter.class.getName()).log(Level.SEVERE, null, ex);
        }
    }
    
    public void send(OtpErlangTuple tuple, OtpMbox mailbox) {
        mailbox.send("pong", server, tuple);
    }

    public OtpErlangObject receive(OtpMbox mailbox) {
        while (true) {
            try {
                return mailbox.receive();

            } catch (OtpErlangExit e) {
                e.printStackTrace();
                break;
            } catch (OtpErlangDecodeException e) {
                e.printStackTrace();
            }
        }
        return null;
    }
}
