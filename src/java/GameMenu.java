package gui;

import utils.Utils;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import theGame.*;

@SuppressWarnings("serial")
public class GameMenu extends JFrame
{
	private JMenuBar menuBar;
	private JMenu menu, submenu;
	private JMenuItem menuItem;
	private JButton gameButton, connectButton, sendButton;
	private JPanel mainPanel;
	private JTextArea chatOutput;
	private JScrollPane chatScrollPane;
	private JTextField chatInput;
	
	
	public GameMenu()
	{
		super("Yolo");
		init_menubar();
		init_content();
		setJMenuBar(menuBar);

		
		setVisible(true);
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		setSize(550,400);
		setResizable(false);
		setLocationRelativeTo(null);
		addKeyListener(new WindowListener());
		
	}
	
	private void init_content()
	{
		mainPanel = new JPanel();
		getContentPane().add(mainPanel);
		mainPanel.setLayout(null);
		
		ImageIcon iconGames = new ImageIcon(getClass().getResource("images/games.gif"));
		gameButton = new JButton("Games",iconGames);
		gameButton.setBounds(10,10,128,128);
		gameButton.addActionListener(new ButtonListener());
		
		mainPanel.add(gameButton);
		
		ImageIcon iconBomber = new ImageIcon(getClass().getResource("images/bomberman.png"));
		gameButton = new JButton(iconBomber);
		gameButton.setBounds(148,10,128,128);
		gameButton.addActionListener(new ButtonListener());
		
		mainPanel.add(gameButton);
		
		ImageIcon iconChess = new ImageIcon(getClass().getResource("images/chess.png"));
		gameButton = new JButton(iconChess);
		gameButton.setBounds(10,148,128,128);
		gameButton.addActionListener(new ButtonListener());
		
		mainPanel.add(gameButton);
		
		ImageIcon iconTroll = new ImageIcon(getClass().getResource("images/troll.png"));
		gameButton = new JButton(iconTroll);
		gameButton.setBounds(148,148,128,128);
		gameButton.addActionListener(new ButtonListener());
		
		mainPanel.add(gameButton);
		
		connectButton = new JButton("Connect to Server!");
		connectButton.setBounds(10,286,266,50);
		connectButton.addActionListener(new ButtonListener());
		
		mainPanel.add(connectButton);
		
		chatOutput = new JTextArea(10,16);
		chatOutput.setEditable(false);
		chatOutput.setBackground(Color.WHITE);
		chatScrollPane = new JScrollPane(chatOutput);
		chatScrollPane.setBounds(286,10,250,266);
		
		mainPanel.add(chatScrollPane);
		
		chatInput = new JTextField(10);
		chatInput.setBounds(286,286,160,50);
		chatInput.addKeyListener(new WindowListener());
		
		
		mainPanel.add(chatInput);
		
		sendButton = new JButton("Send");
		sendButton.addActionListener(new ButtonListener());
		sendButton.setBounds(456,286,80,50);
		
		mainPanel.add(sendButton);
	}
	
	private void init_menubar()
	{
		menuBar = new JMenuBar();
		menu = new JMenu("File");
		menu.setMnemonic(KeyEvent.VK_ALT);
		menuBar.add(menu);

		//a group of JMenuItems
		menuItem = new JMenuItem("Settings", KeyEvent.VK_1);
		menuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_1, ActionEvent.ALT_MASK));
		menuItem.addActionListener(new ButtonListener());
		menu.add(menuItem);

		menuItem = new JMenuItem("Exit");
		menuItem.setMnemonic(KeyEvent.VK_X);
		menuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_X, ActionEvent.ALT_MASK));
		menuItem.addActionListener(new ButtonListener());
		menu.add(menuItem);
		
		//a submenu
		menu.addSeparator();
		submenu = new JMenu("Submenu");
		submenu.setMnemonic(KeyEvent.VK_S);

		menuItem = new JMenuItem("Pancakes");
		menuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_2, ActionEvent.ALT_MASK));
		submenu.add(menuItem);

		menuItem = new JMenuItem("Saft");
		submenu.add(menuItem);
		menu.add(submenu);

		menu = new JMenu("Help");
		menu.setMnemonic(KeyEvent.VK_N);

		menuItem = new JMenuItem("Info");
		menuItem.setMnemonic(KeyEvent.VK_F1);
		menuItem.addActionListener(new ButtonListener());
		menu.add(menuItem);
		menu.addKeyListener(new WindowListener());
		
		menuBar.add(menu);
	}
	
	
	
	public void go()
	{
		System.out.println("Running!");
	}
	
	public void settings()
	{
		System.out.println("TBfuckingI.");
	}
	
	public void info()
	{
		JOptionPane.showMessageDialog(this, "Looks like you have problems, do you want some help?");
	}
	
	
	private class ButtonListener implements ActionListener
	{
		@Override
		public void actionPerformed(ActionEvent e) 
		{
			switch(((AbstractButton)e.getSource()).getText())
			{
				case "Games":
					new GameControl("Henrik","Niklas");
					break;
				case "Send":
					Utils.updateChat(chatOutput,chatInput);
					break;
				case "Connect to Server!":
					Utils.serverConnect();
					break;
				case "Settings":
					settings();
					break;
				case "Exit":
					System.exit(0);
					break;
				case "Info":
					info();
					break;
				default:
					System.out.println("Not implemented yet");
					break;
			}	
		}
	}
	
	private class WindowListener implements KeyListener
	{
		@Override
		public void keyPressed(KeyEvent e) {
			// TODO Auto-generated method stub
			switch(e.getKeyCode())
			{
				case KeyEvent.VK_ENTER:
					Utils.updateChat(chatOutput,chatInput);
					break;
				case KeyEvent.VK_F1:
					info();
					break;
				default:
					
					break;
			}
		}
	
		@Override
		public void keyReleased(KeyEvent arg0) {
			// TODO Auto-generated method stub
			
		}
	
		@Override
		public void keyTyped(KeyEvent arg0) {
			// TODO Auto-generated method stub
			
		}
	}
}