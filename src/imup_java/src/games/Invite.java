package games;

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

/**
 *
 * @author Tobias
 */
import java.awt.BorderLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;

public class Invite {
    private String inviter, game;
    
    public Invite(String inviter, String game) {
        this.inviter = inviter;
        this.game = game;
        init_GUI();
    }
    
    private void init_GUI() {
        JFrame guiFrame = new JFrame();
        JPanel guiPanel = new JPanel(new GridBagLayout());
        JLabel inviterLabel = new JLabel(inviter);
        JLabel inviteLabel = new JLabel(" has invited you to play");
        JLabel gameLabel = new JLabel(game);
        JButton join = new JButton(" join ");
        JButton cancel = new JButton("cancel");
        guiFrame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        guiFrame.setTitle("Invitation");
        guiFrame.setSize(200, 170);
        guiFrame.setLocationRelativeTo(null);
        GridBagConstraints labelGBC = new GridBagConstraints();
        labelGBC.insets = new Insets(10, 8, 10, 10);
        GridBagConstraints fieldGBC = new GridBagConstraints();
        fieldGBC.insets = new Insets(3, 3, 3, 3);
        fieldGBC.gridwidth = GridBagConstraints.REMAINDER;
        guiPanel.add(inviterLabel, fieldGBC);
        guiPanel.add(inviteLabel, fieldGBC);
        guiPanel.add(gameLabel, fieldGBC);
        guiPanel.add(join, labelGBC);
        guiPanel.add(cancel, fieldGBC);
        guiFrame.add(guiPanel, BorderLayout.NORTH);
        guiFrame.setVisible(true);
    }
}
