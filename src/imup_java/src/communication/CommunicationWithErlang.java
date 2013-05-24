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
import java.util.*;

public class CommunicationWithErlang {

    static String client;
    static String cookie;

    public CommunicationWithErlang(String playerID) {
        client = "client" + playerID;
        cookie = "cookie" + playerID;
    }

    public OtpMbox createMailbox(String gameID, String playerID) {
        OtpNode self = null;
        OtpMbox mbox = null;
        try {
            self = new OtpNode(gameID, cookie);
            mbox = self.createMbox("facserver");
            send_messagePing(gameID, playerID, mbox);
            if (self.ping(client, 2000)) {
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

    public void runSnameTerminalStuff() {
        final String cmdLine = "erl -shell -sname " + client + " -setcookie " + cookie;
        //final String cmdLine = "erl -shell -sname " + client + " -setcookie " + cookie + " -s client";

        try {
            Process p = Runtime.getRuntime().exec(cmdLine);
            //Process p1 = Runtime.getRuntime().exec("cmd /c" + cmdLine1);
        } catch (IOException ioe) {
        }
    }

    private void send_messagePing(String gameID, String playerID, OtpMbox mailbox) {
        OtpErlangObject[] msg = new OtpErlangObject[4];
        msg[0] = mailbox.self();
        msg[1] = new OtpErlangAtom(gameID);
        msg[2] = new OtpErlangAtom(playerID);
        msg[3] = new OtpErlangAtom("ping");
        OtpErlangTuple tuple = new OtpErlangTuple(msg);
        //Nedanstående rader finns bara för att kunna kolla att pong skickas
        //tillbaka för att kontrollera.
        mailbox.send("pong", client, tuple);
        try {
            mailbox.receive();
        } catch (OtpErlangExit ex) {
            Logger.getLogger(CommunicationWithErlang.class
                    .getName()).log(Level.SEVERE, null, ex);
        } catch (OtpErlangDecodeException ex) {
            Logger.getLogger(CommunicationWithErlang.class
                    .getName()).log(Level.SEVERE, null, ex);
        }
    }

    public void send(OtpErlangTuple tuple, OtpMbox mailbox) {
        mailbox.send("pong", client, tuple);
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
