package org.jboss.tools.runtime.as.ui.bot.test.dialog.preferences;

import org.eclipse.swtbot.swt.finder.exceptions.WidgetNotFoundException;
import org.jboss.tools.ui.bot.ext.SWTBotExt;
import org.jboss.tools.ui.bot.ext.SWTBotFactory;
import org.jboss.tools.ui.bot.ext.gen.ActionItem.Preference;
import org.jboss.tools.ui.bot.ext.gen.IPreference;

public class PreferencesDialog {

	protected void open(String... path){
		SWTBotExt bot = SWTBotFactory.getBot();
		try {
			bot.shell("Preferences");
			ok();
		} catch (WidgetNotFoundException e){
			// ok
		}
		IPreference preference = Preference.create(path);
		SWTBotFactory.getOpen().preferenceOpen(preference);
	}
	
	public void ok(){
		SWTBotFactory.getBot().button("OK").click();
	}
}
