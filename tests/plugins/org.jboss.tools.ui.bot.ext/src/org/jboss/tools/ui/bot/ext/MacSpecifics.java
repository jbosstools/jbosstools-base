package org.jboss.tools.ui.bot.ext;

import org.apache.log4j.Logger;
import org.eclipse.swtbot.swt.finder.SWTBot;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTable;
import org.jboss.tools.ui.bot.ext.gen.ActionItem;
import org.jboss.tools.ui.bot.ext.types.IDELabel;

public class MacSpecifics {

	private static final Logger log = Logger.getLogger(MacSpecifics.class);
	
	public static final void setupToolkit(){
		 if (SWTJBTExt.isRunningOnMacOs()){
			 setupToolkitInternal();
		 }
	}
	
	public static final void setupJava(){
		if (SWTJBTExt.isRunningOnMacOs()){
			setupJavaInternal();
		}
	}
	
	private static final void setupToolkitInternal(){
		 String javaVersion = System.getProperty("java.specification.version");
		  log.info("Is running on MacOS: " + SWTJBTExt.isRunningOnMacOs());
		  log.info("Java version: " + javaVersion);
		  if ("1.7".equals(javaVersion)){
			  log.info("default AWT toolkit: " + System.getProperty("awt.toolkit"));
			  System.setProperty("awt.toolkit", "sun.lwawt.macosx.LWCToolkit");
			  log.info("AWT toolkit changed to: " + System.getProperty("awt.toolkit"));
		  }
	}
	
	public static final void setupJavaInternal(){
		SWTBot bot = SWTBotFactory.getOpen().preferenceOpen(ActionItem.Preference.JavaInstalledJREs.LABEL);
		setDefaultJava(bot);
		SWTBotFactory.getOpen().finish(bot, IDELabel.Button.OK);
	}
	
	private static void setDefaultJava(SWTBot bot) {
		String javaLocation = System.getProperty("java.home");
		SWTBotTable table = bot.table();
		
		for (int i = 0; i < table.rowCount(); i++){
			if (javaLocation.equals(table.cell(i, 1))){
				table.getTableItem(i).check();			
				return;
			}
		}
		
		throw new IllegalStateException("No java with required location " + javaLocation + " is defined");
	}
}
