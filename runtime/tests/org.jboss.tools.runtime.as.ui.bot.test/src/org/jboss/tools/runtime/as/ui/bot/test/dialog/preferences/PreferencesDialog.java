package org.jboss.tools.runtime.as.ui.bot.test.dialog.preferences;

import org.apache.log4j.Logger;
import org.eclipse.swtbot.swt.finder.exceptions.WidgetNotFoundException;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotShell;
import org.jboss.tools.ui.bot.ext.SWTBotExt;
import org.jboss.tools.ui.bot.ext.SWTBotFactory;
import org.jboss.tools.ui.bot.ext.gen.ActionItem.Preference;
import org.jboss.tools.ui.bot.ext.gen.IPreference;

public class PreferencesDialog {

	private static final Logger log = Logger.getLogger(PreferencesDialog.class);
	
	protected void open(String... path){
		SWTBotExt bot = SWTBotFactory.getBot();
		try {
			bot.shell("Preferences");
			ok();
		} catch (WidgetNotFoundException e){
			// ok
		}
		activateWorkbenchShell();
		IPreference preference = Preference.create(path);
		SWTBotFactory.getOpen().preferenceOpen(preference);
	}
	
	public void activateWorkbenchShell(){
		log.info("Trying to activate workbench shell");
		SWTBotShell[] shells = SWTBotFactory.getBot().shells();
		if (shells.length == 1){
			log.info("Only one shell present, assuming it's workbench and activating");
			shells[0].activate();
		} else {
			log.info("More than one shell present");
			for (SWTBotShell shell : shells){
				log.info(shell.getText());
			}
		}
	}
	
	public void ok(){
		SWTBotFactory.getBot().button("OK").click();
	}
}
