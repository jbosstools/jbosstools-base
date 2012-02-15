/*******************************************************************************
 * Copyright (c) 2007-2011 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributor:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/

package org.jboss.tools.ui.bot.ext.helper;

import static org.junit.Assert.assertTrue;

import java.io.File;

import org.eclipse.swtbot.swt.finder.exceptions.WidgetNotFoundException;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotButton;
import org.jboss.tools.ui.bot.ext.SWTBotExt;
import org.jboss.tools.ui.bot.ext.SWTEclipseExt;
import org.jboss.tools.ui.bot.ext.SWTUtilExt;
import org.jboss.tools.ui.bot.ext.Timing;
import org.jboss.tools.ui.bot.ext.types.IDELabel;

/**
 * Helper for managing project build path
 * 
 * @author Vlado Pakan
 * 
 */
public class BuildPathHelper {
	
	/**
	 * Add External Jar File to Project Build Path. If External Jar File already
	 * exists method will fail
	 * 
	 * @param externalJarLocation
	 * @param projectName
	 * @return
	 */
	public static String addExternalJar(final String externalJarLocation,
			final String projectName) {

		return addExternalJar(externalJarLocation, projectName, false);

	}
	
	/**
	 * Adds External Jar File to Project Build Path. If External Jar File already
	 * exists and 'overwriteIfExists' parameter is set to true, it is overwritten
	 * 
	 * @param externalJarLocation
	 * @param projectName
	 * @return
	 */
	public static String addExternalJar(final String externalJarLocation,
			final String projectName, boolean overwriteIfExists) {
		assertTrue("External Jar Location cannot be empty but is " + externalJarLocation, 
				externalJarLocation != null && externalJarLocation.length() > 0);
		SWTBotExt bot = new SWTEclipseExt().openPropertiesOfProject(projectName);
    bot.shell(IDELabel.Shell.PROPERTIES_FOR + " " + projectName).activate().bot();
		bot.tree().expandNode(IDELabel.JavaBuildPathPropertiesEditor.JAVA_BUILD_PATH_TREE_ITEM_LABEL).select();
    bot.sleep(Timing.time3S());		
		bot.tabItem(IDELabel.JavaBuildPathPropertiesEditor.LIBRARIES_TAB_LABEL).activate();
		final SWTBotButton btn = bot.button(IDELabel.Button.ADD_VARIABLE);
		btn.click();
		bot.sleep(Timing.time2S());
		// workaround because first click is not working when test is run via maven
		try {
		  bot.shell(IDELabel.Shell.NEW_VARIABLE_CLASS_PATH_ENTRY).activate();  
		} catch (WidgetNotFoundException wnfe){
		  btn.click();  
		  bot.sleep(Timing.time2S());
		  bot.shell(IDELabel.Shell.NEW_VARIABLE_CLASS_PATH_ENTRY).activate();
		}
		String jarFileName = new File(externalJarLocation).getName();
		String variableEntryName = jarFileName.toUpperCase() + "_LOCATION";	
		boolean externalJarExists = false;
		for (int i = 0; i < bot.table().rowCount(); i++) {
			if (bot.table().getTableItem(i).getText().split(" - ")[0].equals(variableEntryName)) {
				bot.table().getTableItem(i).select();
				externalJarExists = true;
				break;
			}
		}
		bot.button(IDELabel.Button.CONFIGURE_VARIABLES).click();
		bot.shell(IDELabel.Shell.PREFERENCES_FILTERED).activate();
		if (externalJarExists && overwriteIfExists) {
			bot.button(IDELabel.Button.EDIT).click();					
			bot.shell(IDELabel.Shell.EDIT_VARIABLE_ENTRY).activate();	
			bot.textWithLabel(IDELabel.NewVariableEntryDialog.PATH_TEXT_LABEL)
					.setText(externalJarLocation);
		} else {
			bot.button(IDELabel.Button.NEW).click();
			bot.shell(IDELabel.Shell.NEW_VARIABLE_ENTRY).activate();		
			bot.textWithLabel(IDELabel.NewVariableEntryDialog.NAME_TEXT_LABEL)
					.setText(variableEntryName);		
			bot.textWithLabel(IDELabel.NewVariableEntryDialog.PATH_TEXT_LABEL)
					.setText(externalJarLocation);
		}
		bot.clickButton(IDELabel.Button.OK).click();
		String result = TableHelper.getSelectionText(bot.table());
		bot.clickButton(IDELabel.Button.OK).click();
		bot.clickButton(IDELabel.Button.OK).click();
		bot.clickButton(IDELabel.Button.OK).click();
		new SWTUtilExt(bot).waitForNonIgnoredJobs();
		return result;
	}

	/**
	 * Removes variable from project classpath
	 * 
	 * @param variableLabel
	 * @param removeGlobaly
	 */
	public static void removeVariable(String projectName, String variableLabel,
			boolean removeGlobaly) {
		SWTBotExt bot = new SWTEclipseExt().openPropertiesOfProject(projectName);
		bot.tree().expandNode(IDELabel.JavaBuildPathPropertiesEditor.JAVA_BUILD_PATH_TREE_ITEM_LABEL).select();
		bot.tabItem(IDELabel.JavaBuildPathPropertiesEditor.LIBRARIES_TAB_LABEL).activate();
		bot.tree(1).select(variableLabel);
		bot.button(IDELabel.Button.REMOVE).click();
		if (removeGlobaly) {
			bot.button(IDELabel.Button.ADD_VARIABLE).click();
			bot.shell(IDELabel.Shell.NEW_VARIABLE_CLASS_PATH_ENTRY).activate();
			bot.table().select(variableLabel);
			bot.button(IDELabel.Button.CONFIGURE_VARIABLES).click();
			bot.shell(IDELabel.Shell.PREFERENCES_FILTERED).activate();
			bot.button(IDELabel.Button.REMOVE).click();
			bot.button(IDELabel.Button.OK).click();
			bot.shell(IDELabel.Shell.CLASSPATH_VARIABLES_CHANGED).activate();
			bot.button(IDELabel.Button.YES).click();
			new SWTUtilExt(bot).waitForNonIgnoredJobs();
			bot.clickButton(IDELabel.Button.CANCEL).click();
		}
		bot.button(IDELabel.Button.OK).click();
		new SWTUtilExt(bot).waitForNonIgnoredJobs();
	}

}
