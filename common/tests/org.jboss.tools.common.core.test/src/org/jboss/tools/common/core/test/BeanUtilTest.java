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
		checkBeanName(".", "");
		checkBeanName("ShortName.", "");
		checkBeanName("A", "a");
		checkBeanName("ShortName", "shortName");
		checkBeanName("org.jboss.tools.common.FullyQualifiedName", "fullyQualifiedName");
		checkBeanName("o.j.t.c.FullyQualifiedName", "fullyQualifiedName");
		checkBeanName("....FullyQualifiedName", "fullyQualifiedName");
	}
	
	@Test
	public void testClassName(){
		checkClassName("", "");
		checkClassName(".", ".");
		checkClassName("a", "A");
		checkClassName("shortName", "ShortName");
		checkClassName("shortName.", "shortName.");
		checkClassName("org.jboss.tools.common.fullyQualifiedName", "org.jboss.tools.common.FullyQualifiedName");
		checkClassName("o.j.t.c.fullyQualifiedName", "o.j.t.c.FullyQualifiedName");
		checkClassName("....fullyQualifiedName", "....FullyQualifiedName");
	}
	
	private void checkBeanName(String className, String beanName){
		String result = BeanUtil.getDefaultBeanName(className);
		assertEquals("BeanUtil.getDefaultBeanName returned wrong Bean Name", beanName, result);
	}
	
	private void checkClassName(String beanName, String className){
		String result = BeanUtil.getClassName(beanName);
		assertEquals("BeanUtil.getClassName returned wrong Java Class Name", className, result);
	}
}
