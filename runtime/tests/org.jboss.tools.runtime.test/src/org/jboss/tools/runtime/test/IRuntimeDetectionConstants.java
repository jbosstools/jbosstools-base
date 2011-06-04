/*************************************************************************************
 * Copyright (c) 2011 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.jboss.tools.runtime.test;

/**
 * 
 * @author snjeza
 *
 */
public interface IRuntimeDetectionConstants {
	
	final static String JBOSS_42_HOME = System.getProperty("jbosstools.test.jboss.home.4.2", "E:\\JBossRuntimes\\jboss-4.2.3.GA");

	final static String JBOSS_51_HOME = System.getProperty("jbosstools.test.jboss.home.5.1", "E:\\JBossRuntime2\\jboss-5.1.0.GA");

	final static String JBOSS_70_HOME = System.getProperty("jbosstools.test.jboss.home.7.0", "E:\\jboss-7.0.0.Beta3");

	final static String SEAM_20_HOME = System.getProperty("jbosstools.test.seam.2.0.1.GA.home", "E:\\JBossRuntimes\\jboss-seam-2.0.1.GA");

	final static String SEAM_22_HOME = System.getProperty("jbosstools.test.seam.2.2.0.GA.home", "E:\\JBossRuntime2\\jboss-seam-2.2.1.Final");

	final static String EAP_43_HOME = System.getProperty("jbosstools.test.eap.4.3.home", "E:\\jboss-eap43_cp03");

}
