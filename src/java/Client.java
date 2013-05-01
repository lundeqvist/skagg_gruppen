/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package projekt;

/**
 *
 * @author Linda
 */
public class Client {

    static Converter start_gui_converter;
    static gui.GameMenu gMenu;
    static utils.Utils utils;
    
    public static void main(String[] _args) {
        start_gui_converter = new Converter();
        start_gui_converter.send_message("erlang", "startUp", "ping");
        gMenu = new gui.GameMenu();
    }

}
