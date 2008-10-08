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
package org.jboss.tools.common.el.core.test;

import java.util.List;

import org.jboss.tools.common.el.core.model.ELExpression;
import org.jboss.tools.common.el.core.model.ELInstance;
import org.jboss.tools.common.el.core.model.ELMethodInvocation;
import org.jboss.tools.common.el.core.model.ELModel;
import org.jboss.tools.common.el.core.model.ELMultiExpression;
import org.jboss.tools.common.el.core.model.ELObject;
import org.jboss.tools.common.el.core.model.ELObjectType;
import org.jboss.tools.common.el.core.model.ELParameters;
import org.jboss.tools.common.el.core.parser.ELParser;
import org.jboss.tools.common.el.core.parser.ELParserUtil;

import junit.framework.TestCase;

public class ELModelTest extends TestCase {

	protected void setUp() throws Exception {
	}
	
	public void testELModel() {
		ELParser parser = ELParserUtil.getJbossFactory().createParser();
		String el = "#{a.b(c.d(),\"ooo\",s['h'])}xx#{(18 + a) * d + 14}";
		ELModel model = parser.parse(el);
		
		List<ELInstance> instances = model.getInstances();
		
		assertEquals(2, instances.size());
		
		ELInstance instance_1 = instances.get(0);
		ELExpression expr1 = instance_1.getExpression();
		assertTrue(expr1 instanceof ELMethodInvocation);

		ELMethodInvocation method1 = (ELMethodInvocation)expr1;
		ELParameters paramsObject1 = method1.getParameters();
		List<ELExpression> paramsList1 = paramsObject1.getParameters();
		assertEquals(3, paramsList1.size());

		ELExpression param_1_1 = paramsList1.get(0);
		assertEquals(ELObjectType.EL_METHOD_INVOCATION, param_1_1.getType());

		ELExpression param_1_2 = paramsList1.get(1);
		assertEquals(ELObjectType.EL_VALUE, param_1_2.getType());

		ELExpression param_1_3 = paramsList1.get(2);
		assertEquals(ELObjectType.EL_ARGUMENT_INVOCATION, param_1_3.getType());

		
		ELInstance instance_2 = instances.get(1);
		ELExpression expr2 = instance_2.getExpression();
		assertEquals(ELObjectType.EL_MULTI_EXPRESSION, expr2.getType());
		
		List<ELExpression> exprList2 = ((ELMultiExpression)expr2).getExpressions();
		assertEquals(3, exprList2.size());
		ELExpression expr_2_1 = exprList2.get(0);
		assertEquals(ELObjectType.EL_COMPLEX_EXPRESSION, expr_2_1.getType());
	}

}
