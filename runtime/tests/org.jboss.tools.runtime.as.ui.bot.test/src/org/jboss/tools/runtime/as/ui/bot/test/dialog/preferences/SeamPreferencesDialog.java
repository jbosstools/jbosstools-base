package org.jboss.tools.runtime.as.ui.bot.test.dialog.preferences;

import java.awt.AWTException;
import java.awt.Robot;
import java.awt.event.KeyEvent;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.swtbot.swt.finder.SWTBot;
import org.eclipse.swtbot.swt.finder.keyboard.KeyboardFactory;
import org.eclipse.swtbot.swt.finder.keyboard.Keystrokes;
import org.eclipse.swtbot.swt.finder.utils.SWTBotPreferences;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTable;
import org.jboss.tools.runtime.as.ui.bot.test.entity.Runtime;
import org.jboss.tools.ui.bot.ext.SWTBotFactory;

public class SeamPreferencesDialog extends PreferencesDialog {

	public void open(){
		open("JBoss Tools", "Web", "Seam");
	}
	
	public List<Runtime> getRuntimes(){
		List<Runtime> runtimes = new ArrayList<Runtime>();
		
		SWTBotTable table = SWTBotFactory.getBot().table();
		
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
		SWTBotPreferences.TIMEOUT = 10000;
		SWTBot bot = SWTBotFactory.getBot();
		SWTBotTable table = bot.table();
		
		int runtimesNumber = table.rowCount();
		for (int i = 0; i < runtimesNumber; i++){
			table.click(0, 0);
			bot.button("Remove").click();
			Robot robot = null;
			try {
				robot = new Robot();
			} catch (AWTException e) {
				e.printStackTrace();
			}
			robot.keyPress(KeyEvent.VK_RIGHT);
			robot.keyRelease(KeyEvent.VK_ENTER);
			robot.keyPress(KeyEvent.VK_ENTER);
			robot.keyRelease(KeyEvent.VK_ENTER);
			//KeyboardFactory.getAWTKeyboard().pressShortcut(Keystrokes.RIGHT, Keystrokes.CR, Keystrokes.LF);
			bot.shell("Preferences").activate();
		}
	}
}
