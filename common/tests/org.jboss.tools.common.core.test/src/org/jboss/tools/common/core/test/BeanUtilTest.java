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

import static org.junit.Assert.assertEquals;

import org.jboss.tools.common.util.BeanUtil;
import org.junit.Test;
/**
 * 
 * @author Daniel Azarov
 *
 */
public class BeanUtilTest {
	@Test
	public void testDefaultBeanName(){
		checkBeanName("", "");
		checkBeanName("A", "a");
		checkBeanName("ShortName", "shortName");
		checkBeanName("org.jboss.tools.common.FullyQualifiedName", "fullyQualifiedName");
		checkBeanName("o.j.t.c.FullyQualifiedName", "fullyQualifiedName");
		checkBeanName("....FullyQualifiedName", "fullyQualifiedName");
	}
	
	private void checkBeanName(String className, String beanName){
		String result = BeanUtil.getDefaultBeanName(className);
		assertEquals("BeanUtil.getDefaultBeanName returned wrong Bean Name", beanName, result);
	}
}
