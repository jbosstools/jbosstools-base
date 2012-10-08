package org.jboss.tools.runtime.as.ui.bot.test.dialog.preferences;

import java.awt.AWTException;
import java.awt.Dimension;
import java.awt.Robot;
import java.awt.Toolkit;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.List;

import org.jboss.reddeer.eclipse.jface.preference.PreferencePage;
import org.jboss.reddeer.swt.api.Table;
import org.jboss.reddeer.swt.condition.JobIsRunning;
import org.jboss.reddeer.swt.condition.WaitCondition;
import org.jboss.reddeer.swt.exception.WidgetNotAvailableException;
import org.jboss.reddeer.swt.impl.button.PushButton;
import org.jboss.reddeer.swt.impl.label.DefaultLabel;
import org.jboss.reddeer.swt.impl.table.DefaultTable;
import org.jboss.reddeer.swt.wait.TimePeriod;
import org.jboss.reddeer.swt.wait.WaitUntil;
import org.jboss.reddeer.swt.wait.WaitWhile;
import org.jboss.tools.runtime.as.ui.bot.test.entity.Runtime;

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
			new WaitWhile(new JobIsRunning());
			new WaitUntil(new RemoveButtonEnabled(),TimePeriod.LONG);			
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
	private static class RemoveButtonEnabled implements WaitCondition {

		@Override
		public boolean test() {
			try {
				PushButton removeButton = new PushButton("Remove");
				return removeButton.isEnabled();
			} catch (WidgetNotAvailableException e){
				return false;
			}
		}

		@Override
		public String description() {
			return "The runtime search has not finished in the specified amount of time";
		}
	}
}
