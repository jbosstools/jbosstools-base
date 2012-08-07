package org.jboss.tools.dummy.ui.bot.test;


import static org.eclipse.swtbot.swt.finder.waits.Conditions.shellIsActive;
import static org.junit.Assert.assertEquals;

import java.io.OutputStreamWriter;

import org.apache.log4j.ConsoleAppender;
import org.apache.log4j.Logger;
import org.apache.log4j.PatternLayout;
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
	
	static Logger log = Logger.getLogger(DummyTest.class);
	
	@BeforeClass
	public static void before() {		
		ConsoleAppender ca = new ConsoleAppender();
		ca.setWriter(new OutputStreamWriter(System.out));
		ca.setLayout(new PatternLayout("[%-5p] %t: %m%n"));
		log.addAppender(ca);
	}

	@Test
	public void dummyTest() {
		Logger log = Logger.getLogger(this.getClass());
		ConsoleAppender ca = new ConsoleAppender();
		ca.setWriter(new OutputStreamWriter(System.out));
		ca.setLayout(new PatternLayout("[%-5p] %t: %m%n"));
		log.addAppender(ca);

		String pref = "Preferences";
		SWTWorkbenchBot bot = new SWTWorkbenchBot();
		bot.menu("Window").menu(pref).click();
		bot.waitUntil(shellIsActive(pref), 10000);
		SWTBotShell shell = bot.shell(pref);
		assertEquals(pref,shell.getText());
		bot.activeShell().close();
	}
	
	@Test
	public void hundredTimes() {
		for (int i = 0 ; i < 100; i++)
			dummyTest();
	}
	
	@AfterClass
	public static void after() {
		SWTWorkbenchBot bot = new SWTWorkbenchBot();
		bot.closeAllShells();
	}
}
