/******************************************************************************* 
 * Copyright (c) 2012 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 *     Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/
package org.jboss.tools.tests.installation;

import org.eclipse.swtbot.eclipse.finder.SWTBotEclipseTestCase;
import org.eclipse.swtbot.swt.finder.SWTBot;
import org.eclipse.swtbot.swt.finder.waits.ICondition;
import org.junit.Test;

/**
 * @author mistria
 */
public class CheckForUpdatesTest extends SWTBotEclipseTestCase {

	@Test
	public void testCheckForUpdates() throws Exception {
		bot.menu("Help").menu("Check for Updates").click();
		bot.shell("Contacting Software Sites");
		this.bot.waitWhile(new ICondition() {
			@Override
			public boolean test() throws Exception {
				return bot.activeShell().getText().equals("Contacting Software Sites");
			}
			
			@Override
			public void init(SWTBot bot) {
			}
			
			@Override
			public String getFailureMessage() {
				return "Blocking while calculating deps";
			}
		}, 10 * 60000); // 5 minutes timeout
		InstallTest.continueInstall(bot);
	}
}
