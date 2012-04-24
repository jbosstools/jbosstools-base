 /*******************************************************************************
  * Copyright (c) 2007-2012 Red Hat, Inc.
  * Distributed under license by Red Hat, Inc. All rights reserved.
  * This program is made available under the terms of the
  * Eclipse Public License v1.0 which accompanies this distribution,
  * and is available at http://www.eclipse.org/legal/epl-v10.html
  *
  * Contributor:
  *     Red Hat, Inc. - initial API and implementation
  ******************************************************************************/
package org.jboss.tools.ui.bot.ext.test.config;

import org.jboss.tools.ui.bot.ext.RequirementAwareSuite;
import org.junit.runner.RunWith;
import org.junit.runners.Suite.SuiteClasses;
/**
 * Test suite to test SWT Bot Extension configurations. Some tests are commented out
 * to not run test during automated build.
 * Test which are commented out should be run just manually when changing related configurations
 * @author lzoubek
 *
 */
@SuiteClasses( {
  /*
  ConfigBeanTest.class,
  */
	NotAnnotated.class,
	/*
	ClassWithoutTests.class,
	AnnotatedDefaultServer.class,
	AnnotatedEPPServer.class,
	AnnotatedJbossASServer.class,
	AnnotatedNotRunningServer.class,
	AnnotatedDefaultServer.class,
	AnnotatedWithESB.class,
	AnnotatedDisabledServer.class,
	AnonatedRunningServer.class, 
	AnnotatedWithSeam.class,
	AnnotatedRemoteServer.class,
	AnnotatedLocalServer.class
	*/
})
@RunWith(RequirementAwareSuite.class)
public class Suite {

}
