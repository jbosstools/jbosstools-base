package org.jboss.tools.ui.bot.ext.condition;

import org.eclipse.swtbot.swt.finder.SWTBot;
import org.eclipse.swtbot.swt.finder.exceptions.WidgetNotFoundException;
import org.eclipse.swtbot.swt.finder.finders.UIThreadRunnable;
import org.eclipse.swtbot.swt.finder.results.VoidResult;
import org.eclipse.swtbot.swt.finder.waits.ICondition;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotShell;
import org.jboss.tools.ui.bot.ext.parts.SWTBotBrowserExt;

/**
 * Returns false while the default page of given browser isn't loaded.
 * Also closes "Security Warning" shell, if it opens during loading the page.
 * 
 * @author rhopp
 *
 */

public class BrowserIsLoaded implements ICondition {

	SWTBot bot;
	private SWTBotBrowserExt browser;
	
	public BrowserIsLoaded(SWTBotBrowserExt browser){
		browser.addProgressListenerToBrowser();
		this.browser = browser;
	}
	
	@Override
	public boolean test() throws Exception {
		//Trying whether the security window has appeared
		try{
			SWTBotShell shell = bot.shell("Security Warning");
			shell.close();
		}catch(WidgetNotFoundException wnfe){
		}
		
		return browser.isPageLoaded();
	}

	@Override
	public void init(SWTBot bot) {
		this.bot = bot;
	}

	@Override
	public String getFailureMessage() {
		return null;
	}

}
