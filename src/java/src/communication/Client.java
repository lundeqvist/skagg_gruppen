/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package communication;

import com.ericsson.otp.erlang.OtpMbox;

/**
 *
 * @author Linda
 */
public class Client{

    static Converter converter;
    static games.GameMenu gMenu;
    //static utils.Utils utils;
    static Imup imup;
    static OtpMbox mailbox;

    public static void main(String[] _args) {
        //Thread t = new Thread(new Client());
        //t.start();   
        gMenu = new games.GameMenu();
        converter = new Converter();
        mailbox = converter.createMailbox("startUp");
        converter.send_messagePing("erlang", "startUp", mailbox);
    }

    /*public void run() {
        gMenu = new images.GameMenu();
        converter = new Converter();
        converter.send_messagePing("erlang", "startUp");
    }*/
    
    
}
