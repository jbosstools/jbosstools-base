package org.jboss.tools.dummy.ui.bot.test;


import static org.eclipse.swtbot.swt.finder.waits.Conditions.shellIsActive;
import static org.junit.Assert.assertEquals;

import org.apache.log4j.Logger;
import org.eclipse.swtbot.eclipse.finder.SWTWorkbenchBot;
import org.eclipse.swtbot.swt.finder.junit.SWTBotJunit4ClassRunner;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotShell;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;

/**
 * Dummy bot tests - designed to test jenkins slaves
 * @author jpeterka
 *
 */
@RunWith(SWTBotJunit4ClassRunner.class)
public class DummyTest {
	
	Logger log = Logger.getLogger("");
	
	@BeforeClass
	public static void before() {		
	}

	@Test
	public void dummyTest() {
		String pref = "Preferences";
		String window = "Window";
		if (isOSX()) {
			pref = "Preferences...";
			window = "Eclispe";
		}
		SWTWorkbenchBot bot = new SWTWorkbenchBot();
		bot.menu(window).menu(pref).click();
		bot.waitUntil(shellIsActive(pref), 10000);
		SWTBotShell shell = bot.shell(pref);
		assertEquals(pref,shell.getText());
		bot.activeShell().close();
	}
	
	@Test
	public void hundredTimes() {
		final int cycles = 100;
		for (int i = 0 ; i < cycles ; i++)
		{		
			dummyTest();
			log.info(i+1 + "/" + cycles + " try passed");
		}
	}
	
	@AfterClass
	public static void after() {
		SWTWorkbenchBot bot = new SWTWorkbenchBot();
		bot.closeAllShells();
	}
	
	public boolean isOSX() {
	    String osName = System.getProperty("os.name");
	    return osName.contains("OS X");
	}
}
