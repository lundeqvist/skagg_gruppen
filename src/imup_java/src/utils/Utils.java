package utils;

import com.ericsson.otp.erlang.*;
import java.text.DateFormat;
import java.util.Date;
import javax.swing.*;
import communication.*;

public class Utils {
    
    public static OtpErlangTuple convertToErlang(OtpMbox mailbox, String gameID, String playerID, String arguments) {
        OtpErlangObject[] msg = new OtpErlangObject[3];
        msg[0] = mailbox.self();
        msg[1] = new OtpErlangAtom(gameID);
        msg[2] = new OtpErlangAtom(playerID);
        msg[3] = new OtpErlangAtom(arguments);
        return new OtpErlangTuple(msg);
    }
    
    public static Arguments convertToJava(OtpErlangObject robj) {
        OtpErlangTuple rtuple = (OtpErlangTuple) robj;
        String GameID = rtuple.elementAt(1).toString();
        String PlayerID = rtuple.elementAt(2).toString();
        String ArgumentsString = rtuple.elementAt(3).toString();
        return new Arguments(GameID, PlayerID, ArgumentsString);
    }

    public static Arguments receiveMessage(OtpMbox mailbox, CommunicationWithErlang converter) {
        return convertToJava(converter.receive(mailbox));
    }
    
    public static void sendMessage(OtpMbox mailbox, CommunicationWithErlang converter, String gameID, String playerID, String arguments) {
        converter.send(convertToErlang(mailbox, gameID, playerID, arguments), mailbox);
    }
    
    
    /*public void sendMove(String gameID, String playerID, String actionCommand, OtpMbox mailbox) {
        converter.send(convertToErlang(mailbox, new Arguments(mailbox, gameID, playerID, "{" + actionCommand + "}")), mailbox);
    }

    public void receiveMove() {
        OtpErlangObject temp = converter.receive(mailbox);
        String[] message = convertToJava(temp);
    }
*/
    public static int serverConnect() {
        return 0;
    }

    /*public static String[] tupleToStringArray(OtpErlangTuple tuple) {
        String[] tupleArgs = new String[tuple.arity()];
        for (int i = 0; i < tuple.arity(); i++) {
            tupleArgs[i] = tuple.elementAt(i).toString();
        }
        return tupleArgs;
    }

    public static String[] tupleToStringArray(OtpErlangTuple tuple, int length) {
        String[] tupleArgs = new String[length];
        for (int i = 0; i < length; i++) {
            if (tuple.elementAt(i) instanceof OtpErlangTuple) {
                String[] innerTuple = tupleToStringArray((OtpErlangTuple) tuple.elementAt(i), ((OtpErlangTuple) tuple.elementAt(i)).arity());
                for (int j = i; j < i + innerTuple.length; j++) {
                    tupleArgs[j] = innerTuple[j - i];
                }
                i += innerTuple.length;
            } else {
                tupleArgs[i] = tuple.elementAt(i).toString();
            }
        }
        return tupleArgs;
    }
*/
    public static String[] parseTupleString(String tuple) {
        tuple = tuple.trim();
        tuple = tuple.substring(1, tuple.length() - 1);
        String[] parsedTuple = tuple.split(",");
        for (int i = 0; i < parsedTuple.length; i++) {
            parsedTuple[i] = parsedTuple[i].trim();
        }
        return parsedTuple;
    }
    
    public static int[] splitCoordinates(String coordinates) {
        int[] xy = new int[2];
        xy[0] = Integer.parseInt(coordinates.substring(0, 1));
        xy[1] = Integer.parseInt(coordinates.substring(1, 2));
        return xy;
    }
}
