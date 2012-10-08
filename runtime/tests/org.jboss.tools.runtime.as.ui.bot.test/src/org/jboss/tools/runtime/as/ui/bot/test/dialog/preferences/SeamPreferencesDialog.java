package org.jboss.tools.runtime.as.ui.bot.test.dialog.preferences;

import java.awt.AWTException;
import java.awt.Dimension;
import java.awt.MouseInfo;
import java.awt.Robot;
import java.awt.Toolkit;
import java.awt.Window;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.List;
import org.jboss.reddeer.eclipse.jface.preference.PreferencePage;
import org.jboss.reddeer.swt.api.Table;
import org.jboss.reddeer.swt.impl.button.PushButton;
import org.jboss.reddeer.swt.impl.table.DefaultTable;
import org.jboss.tools.runtime.as.ui.bot.test.entity.Runtime;

import sun.awt.X11.Screen;

public class SeamPreferencesDialog extends PreferencePage {

	public SeamPreferencesDialog(){
		super("JBoss Tools", "Web", "Seam");
	}
	
	public List<Runtime> getRuntimes(){
		List<Runtime> runtimes = new ArrayList<Runtime>();
		
		Table table = new DefaultTable();
		
		for (int i = 0; i < table.rowCount(); i++){
			Runtime runtime = new Runtime();
			runtime.setName(table.cell(i, 1));
			runtime.setVersion(table.cell(i, 2));
			runtime.setLocation(table.cell(i, 3));
			runtimes.add(runtime);
		}
		return runtimes;
	}
	
	public void removeAllRuntimes(){
		Table table = new DefaultTable();
		
		int runtimesNumber = table.rowCount();
		for (int i = 0; i < runtimesNumber; i++){
			table.select(0);
			new PushButton("Remove").click();
			
			try {
				Robot robot = new Robot();
				robot.setAutoWaitForIdle(true);
				Toolkit tools = Toolkit.getDefaultToolkit();
				Dimension dim = tools.getScreenSize();
				robot.mouseMove(((int)dim.getWidth())/2,((int)dim.getHeight()/2 - 60));
				robot.mousePress(MouseEvent.BUTTON1_MASK);
				robot.mouseRelease(MouseEvent.BUTTON1_MASK);
				robot.keyPress(KeyEvent.VK_ENTER);
				robot.keyRelease(KeyEvent.VK_ENTER);
			} catch (AWTException e) {
				throw new RuntimeException("Cannot press shortcut during removing of Seam runtimes", e);
			}
			
			open();
		}
	}
}
