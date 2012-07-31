package org.jboss.tools.dummy.ui.bot.test;


import static org.junit.Assert.assertTrue;

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
		assertTrue(true);
	}

	@Test
	public void dummyTest() {
		assertTrue(true);
	}


}
