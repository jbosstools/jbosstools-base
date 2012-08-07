package org.jboss.tools.dummy.ui.bot.test;


import java.io.OutputStreamWriter;

import org.apache.log4j.ConsoleAppender;
import org.apache.log4j.Logger;
import org.apache.log4j.PatternLayout;
import org.eclipse.swtbot.eclipse.finder.SWTWorkbenchBot;
import org.eclipse.swtbot.swt.finder.junit.SWTBotJunit4ClassRunner;
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
	
	@BeforeClass
	public static void before() {
	}

	@Test
	public void dummyTest() {
		Logger log = Logger.getLogger(this.getClass());
		ConsoleAppender ca = new ConsoleAppender();
		ca.setWriter(new OutputStreamWriter(System.out));
		ca.setLayout(new PatternLayout("[%-5p] %t: %m%n"));
		log.addAppender(ca);

		SWTWorkbenchBot bot = new SWTWorkbenchBot();
		log.info("Shell with title \"" + bot.shells()[0].getText() + "\" found");
	}
}
