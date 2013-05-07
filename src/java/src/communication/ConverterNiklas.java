/*package communication;

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
import static communication.Converter.server;

public class ConverterNiklas {

    static String server = "server";
    Client client = new Client();
    //HashMap<String, OtpMbox> mailboxes = new HashMap<String, OtpMbox>();
    
    
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

    public void send_messagePing(String id, String gameID) {
        if (!mailboxes.containsKey(gameID)) {        
            startup_communication(gameID);
        }
                 
        OtpErlangObject[] msg = new OtpErlangObject[4];
        msg[0] = mailboxes.get(gameID).self();
        msg[1] = new OtpErlangAtom(id);
        msg[2] = new OtpErlangAtom(gameID);        
        msg[3] = new OtpErlangAtom("ping");
        OtpErlangTuple tuple = new OtpErlangTuple(msg);
        mailboxes.get(gameID).send("pong", server, tuple);                        
        receive_message(gameID);
    }

    public void send_invite(String playerId, String invitedPlayerId, String game) {
        OtpErlangObject[] id = new OtpErlangObject[2];
        id[0] = new OtpErlangAtom(playerId);
        id[1] = new OtpErlangAtom(game);
        String send_invite = "send_invite"; 
        OtpErlangObject[] msg = new OtpErlangObject[4];
        msg[0] = mailboxes.get("startUp").self();
        msg[1] = new OtpErlangAtom(send_invite);
        msg[2] = new OtpErlangTuple(id);
        msg[3] = new OtpErlangAtom(invitedPlayerId);
        OtpErlangTuple tuple = new OtpErlangTuple(msg);
        mailboxes.get("startUp").send("pong", server, tuple);  
    }
    
    public void send_replyToInvite(String playerID, String game, String inviterID, String answer){
        OtpErlangObject[] id = new OtpErlangObject[2];
        id[0] = new OtpErlangAtom(playerID);
        id[1] = new OtpErlangAtom(game);
        String send_replyToInvite = "send_replyToInvite";
        OtpErlangObject[] msg = new OtpErlangObject[5];
        msg[0] = mailboxes.get("startUp").self();
        msg[1] = new OtpErlangAtom(send_replyToInvite);
        msg[2] = new OtpErlangTuple(id);
        msg[3] = new OtpErlangAtom(answer);
        OtpErlangTuple tuple = new OtpErlangTuple(msg);
        mailboxes.get("startUp").send("pong", server, tuple);
    }
    
    
    
    
    
    public void send_moveRequest(String playerID, String gameID, String newPositionRequest){

        OtpErlangObject[] id = new OtpErlangObject[2];
        id[0] = new OtpErlangAtom(playerID);
        id[1] = new OtpErlangAtom(gameID);
        String send_moveRequest = "send_moveRequest"; 
        OtpErlangObject[] msg = new OtpErlangObject[4];
        msg[0] = mailboxes.get(gameID).self();
        msg[1] = new OtpErlangAtom(send_moveRequest);
        msg[2] = new OtpErlangTuple(id);
        msg[3] = new OtpErlangAtom(newPositionRequest);
        OtpErlangTuple tuple = new OtpErlangTuple(msg);
        mailboxes.get(gameID).send("host", server, tuple); 
    }
    
    public void send_chatLine(String playerID, String gameID, String chatLine){
        OtpErlangObject[] id = new OtpErlangObject[2];
        id[0] = new OtpErlangAtom(playerID);
        id[1] = new OtpErlangAtom(gameID);
        String send_chatLine = "send_chatLine"; 
        OtpErlangObject[] msg = new OtpErlangObject[4];
        msg[0] = mailboxes.get(gameID).self();
        msg[1] = new OtpErlangAtom(send_chatLine);
        msg[2] = new OtpErlangTuple(id);
        msg[3] = new OtpErlangAtom(chatLine);
        OtpErlangTuple tuple = new OtpErlangTuple(msg);
        mailboxes.get(gameID).send("host", server, tuple);
    }
    
    
    
    /**
     *
     * @param num
     * @param personId
     * @param spelid
     * @param message
     */
   /** public void send_message(int num, String personId, String spelid, String message) {
    *    if (!mailboxes.containsKey(spelid)) {
    *       startup_communication(spelid);
    *      send_message(personId, spelid, "ping");
    *    }
    *    OtpErlangObject[] id = new OtpErlangObject[2];
    *    id[0] = new OtpErlangAtom(personId);
    *    id[1] = new OtpErlangAtom(spelid);
    *    
    *    OtpErlangObject[] msg = new OtpErlangObject[4];
    *    msg[0] = mailboxes.get(spelid).self();
    *    msg[1] = new OtpErlangInt(num);
    *    msg[2] = new OtpErlangTuple(id);
    *    msg[3] = new OtpErlangAtom(message);
    *   OtpErlangTuple tuple = new OtpErlangTuple(msg);
    *    mailboxes.get(spelid).send("pong", server, tuple);
    *    
    * }  

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
*/