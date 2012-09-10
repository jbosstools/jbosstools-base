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
import org.eclipse.swtbot.eclipse.finder.widgets.SWTBotMultiPageEditor;
import org.eclipse.swtbot.swt.finder.SWTBot;
import org.eclipse.swtbot.swt.finder.exceptions.WidgetNotFoundException;
import org.eclipse.swtbot.swt.finder.junit.SWTBotJunit4ClassRunner;
import org.eclipse.swtbot.swt.finder.waits.ICondition;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotCheckBox;
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;

/**
 * This is a bot scenario which performs install through JBoss Central.
 * 
 * @author Mickael Istria
 */
@RunWith(SWTBotJunit4ClassRunner.class)
public class InstallFromCentralTest extends SWTBotEclipseTestCase {
	
	@Test
	public void testInstall() throws Exception {
		bot.menu("Help").menu("JBoss Central").click();
		SWTBotMultiPageEditor centralEditor = (SWTBotMultiPageEditor) bot.multipageEditorByTitle("JBoss Central");
		centralEditor.show();
		centralEditor.activatePage("Software/Update");
		try {
			int i = 0;
			SWTBotCheckBox check = null;
			while ((check = bot.checkBox(i)) != null) {
				if (check.getText() == null || !check.getText().contains("Show Installed")) {
					check.click();
				}
				i++;
			}
		} catch (WidgetNotFoundException ex) {
			// last checkbox
		} catch (IndexOutOfBoundsException ex ) {
			// last checkbox
		}
		bot.button("Install").click();
		this.bot.waitUntil(new ICondition() {
			@Override
			public boolean test() throws Exception {
				return bot.activeShell().getText().equals("Install") || bot.activeShell().getText().equals("Problem Occured");
			}
			
			@Override
			public void init(SWTBot bot) {
			}
			
			@Override
			public String getFailureMessage() {
				return "Blocking while calculating deps";
			}
		}, 10 * 60000); // 5 minutes timeout
		if (bot.activeShell().getText().equals("Problem Occured")) {
			String reason = bot.text().getText();
			Assert.fail("Could not install Central content from " + System.getProperty("org.jboss.tools.central.discovery") + "\n" + reason);
		}
		bot.button("Next >").click();
		InstallTest.continueInstall(bot);
		
	}

}
