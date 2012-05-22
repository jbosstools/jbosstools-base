package org.jboss.tools.runtime.as.ui.bot.test.dialog.preferences;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.swtbot.swt.finder.SWTBot;
import org.eclipse.swtbot.swt.finder.keyboard.KeyboardFactory;
import org.eclipse.swtbot.swt.finder.keyboard.Keystrokes;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTable;
import org.jboss.tools.runtime.as.ui.bot.test.entity.Runtime;
import org.jboss.tools.ui.bot.ext.SWTBotFactory;

public class ServerRuntimesPreferencesDialog extends PreferencesDialog {

	public void open(){
		open("Server", "Runtime Environments");
	}
	
	public List<Runtime> getRuntimes(){
		List<Runtime> runtimes = new ArrayList<Runtime>();
		
		SWTBotTable table = SWTBotFactory.getBot().table();
		
		for (int i = 0; i < table.rowCount(); i++){
			Runtime runtime = new Runtime();
			runtime.setName(table.cell(i, 0));
			runtime.setType(table.cell(i, 1));
			runtimes.add(runtime);
		}
		return runtimes;
	}
	
	public void removeAllRuntimes(){
		SWTBot bot = SWTBotFactory.getBot();
		SWTBotTable table = bot.table();
		
		for (int i = 0; i < table.rowCount(); i++){
			table.click(0, 0);
			bot.button("Remove").click();
			KeyboardFactory.getAWTKeyboard().pressShortcut(Keystrokes.RIGHT, Keystrokes.CR, Keystrokes.LF);
			bot.shell("Preferences").activate();
		}
	}
}
