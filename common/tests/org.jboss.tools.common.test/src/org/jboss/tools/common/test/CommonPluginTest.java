/*******************************************************************************
 * Copyright (c) 2007 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.test;

import org.jboss.tools.common.CommonPlugin;

import junit.framework.TestCase;

/**
 * @author eskimo
 *
 */
public class CommonPluginTest extends TestCase {

	/**
	 * Test method for {@link org.jboss.tools.common.CommonPlugin#getInstance()}.
	 */
	public void testGetInstance() {
		assertNotNull("Common plugin is not loaded", CommonPlugin.getInstance());
	}

	/**
	 * Test method for {@link org.jboss.tools.common.CommonPlugin#getMessage(java.lang.String)}.
	 */
	public void testGetMessage() {
		assertNotNull(CommonPlugin.getMessage("reportingUrl"));
	}

	/**
	 * Test method for {@link org.jboss.tools.common.CommonPlugin#getEnvironment()}.
	 */
	public void testGetEnvironment() {
		assertNotNull(CommonPlugin.getEnvironment());
	}

	/**
	 * Test method for {@link org.jboss.tools.common.CommonPlugin#getDefault()}.
	 */
	public void testGetDefault() {
		assertNotNull(CommonPlugin.getDefault());
	}

	/**
	 * Test method for {@link org.jboss.tools.common.CommonPlugin#getPluginLog()}.
	 */
	public void testGetPluginLog() {
		assertNotNull(CommonPlugin.getPluginLog());
	}

	/**
	 * Test method for {@link org.jboss.tools.common.log.BasePlugin#logError(java.lang.String, java.lang.Throwable)}.
	 */
	public void testLogErrorStringThrowable() {
		CommonPlugin.getPluginLog().logError("Error message", new Throwable("Message"));
	}

	/**
	 * Test method for {@link org.jboss.tools.common.log.BasePlugin#logError(java.lang.String)}.
	 */
	public void testLogErrorString() {
		CommonPlugin.getPluginLog().logError("Error message");
	}

	/**
	 * Test method for {@link org.jboss.tools.common.log.BasePlugin#logError(java.lang.Throwable)}.
	 */
	public void testLogErrorThrowable() {
		CommonPlugin.getPluginLog().logError(new Throwable("Message"));
	}

	/**
	 * Test method for {@link org.jboss.tools.common.log.BasePlugin#logWarning(java.lang.String, java.lang.Throwable)}.
	 */
	public void testLogWarningStringThrowable() {
		CommonPlugin.getPluginLog().logWarning("Message", new Throwable("Message"));
	}

	/**
	 * Test method for {@link org.jboss.tools.common.log.BasePlugin#logWarning(java.lang.String)}.
	 */
	public void testLogWarningString() {
		CommonPlugin.getPluginLog().logWarning("Message");
	}

	/**
	 * Test method for {@link org.jboss.tools.common.log.BasePlugin#logWarning(java.lang.Throwable)}.
	 */
	public void testLogWarningThrowable() {
		CommonPlugin.getPluginLog().logWarning(new Throwable("Message"));
	}

	/**
	 * Test method for {@link org.jboss.tools.common.log.BasePlugin#logInfo(java.lang.String, java.lang.Throwable)}.
	 */
	public void testLogInfoStringThrowable() {
		CommonPlugin.getPluginLog().logInfo("Message", new Throwable("Message"));
	}

	/**
	 * Test method for {@link org.jboss.tools.common.log.BasePlugin#logInfo(java.lang.String)}.
	 */
	public void testLogInfoString() {
		CommonPlugin.getPluginLog().logInfo("message");
	}

}
