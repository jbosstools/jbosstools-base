/*******************************************************************************
 * Copyright (c) 2007 Exadel, Inc. and Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Exadel, Inc. and Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.model.test;

import org.jboss.tools.common.ant.parser.test.AntParserTest;
import org.jboss.tools.common.model.exception.test.DeveloperExceptionTest;
import org.jboss.tools.common.model.util.test.EclipseJavaUtilTest;
import org.jboss.tools.test.util.ProjectImportTestSetup;

import junit.framework.Test;
import junit.framework.TestSuite;
/**
 * @author V.Kabanovich
 *
 */
public class CommonModelAllTests {
	public static final String PLUGIN_ID = "org.jboss.tools.common.model";
	//
	public static Test suite() {
		PaletteLoaderTest.initLogger();

		TestSuite suite = new TestSuite();
		suite.setName("All tests for " + PLUGIN_ID);
		suite.addTestSuite(MetaModelTest.class);
		suite.addTestSuite(ClassPathTest.class);
		suite.addTestSuite(AntParserTest.class);
		suite.addTestSuite(DeveloperExceptionTest.class);
		suite.addTestSuite(XModelTransferBufferTest.class);
		suite.addTestSuite(PropertiesLoaderTest.class);
		suite.addTestSuite(JarAccessTest.class);
		suite.addTestSuite(EclipseJavaUtilTest.class);
		suite.addTestSuite(ResourceAdapterTest.class);
		suite.addTestSuite(PaletteLoaderTest.class);
		suite.addTestSuite(MetaConfigurationLoaderTest.class);

		TestSuite annotationSuite = new TestSuite("Annotation Tests");
		annotationSuite.addTestSuite(AnnotationTest.class);
		ProjectImportTestSetup annotationTestSetup = new ProjectImportTestSetup(annotationSuite,
				PLUGIN_ID + ".test",
				new String[]{"projects/AnnotationTest"},
				new String[]{"AnnotationTest"});
		suite.addTest(annotationTestSetup);

		return suite;
	}
}
