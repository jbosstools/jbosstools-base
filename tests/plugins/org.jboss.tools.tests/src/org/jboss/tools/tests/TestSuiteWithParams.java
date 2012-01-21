/******************************************************************************* 
 * Copyright (c) 2007-2012 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 *     Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/
package org.jboss.tools.tests;

import java.beans.IntrospectionException;
import java.beans.PropertyDescriptor;
import java.beans.Statement;
import java.util.Properties;

import junit.framework.Test;
import junit.framework.TestResult;
import junit.framework.TestSuite;

/**
 * @author eskimo
 *
 */
public class TestSuiteWithParams extends TestSuite {
	
	Properties parameters = new Properties();
	
	public TestSuiteWithParams(Class<?> theClass, Properties params) {
		super(theClass);
		this.parameters.putAll(params);
	}

	@Override
	public void runTest(Test test, TestResult result) {
		
		for (Object property : parameters.keySet()) {
			String propertyName = property.toString();
			try {
				PropertyDescriptor propertyDescr = new PropertyDescriptor(propertyName,test.getClass());
				Statement setPropertyStatement = 
					new Statement(
							test,propertyDescr.getWriteMethod().getName(),new Object[]{parameters.get(property)});
				setPropertyStatement.execute();
			} catch (IntrospectionException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (Exception e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			
		}
		super.runTest(test, result);
	}
	
	
}
