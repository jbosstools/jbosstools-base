/*******************************************************************************
 * Copyright (c) 2014 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.common.core.test;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;

/**
 * @author Alexey Kazakov
 */
@RunWith(Suite.class)
@Suite.SuiteClasses({
	WebUtilsTest.class,
	HttpUtilTest.class,
	BeanUtilTest.class})
public class CommonCoreTestSuite {

}