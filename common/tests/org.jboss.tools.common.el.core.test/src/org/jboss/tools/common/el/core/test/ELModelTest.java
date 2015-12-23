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

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.jboss.tools.common.el.core.model.ELExpression;
import org.jboss.tools.common.el.core.model.ELInstance;
import org.jboss.tools.common.el.core.model.ELInvocationExpression;
import org.jboss.tools.common.el.core.model.ELMethodInvocation;
import org.jboss.tools.common.el.core.model.ELModel;
import org.jboss.tools.common.el.core.model.ELMultiExpression;
import org.jboss.tools.common.el.core.model.ELObject;
import org.jboss.tools.common.el.core.model.ELObjectType;
import org.jboss.tools.common.el.core.model.ELParameters;
import org.jboss.tools.common.el.core.model.ELPropertyInvocation;
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

	public void testInvokingMethodOnExpressionInParenthesis() {
		ELParser parser = ELParserUtil.getJbossFactory().createParser();
		String el = "#{(a.b() + c).d()}";
		ELModel model = parser.parse(el);
		
		List<ELInstance> instances = model.getInstances();
		
		assertEquals(1, instances.size());
		
		ELInstance instance = instances.get(0);
		ELExpression expr = instance.getExpression();

		List<ELObject> cs = expr.getChildren();
		assertEquals(2, cs.size());
		
		
		assertEquals("(a.b()+c)", cs.get(0).toString());
		List<ELObject> cs1 = cs.get(0).getChildren();
		assertEquals(1, cs1.size());
		ELExpression expr1 = (ELExpression)cs1.get(0);
		assertEquals("a.b()+c", expr1.toString());
		List<ELObject> cs2 = expr1.getChildren();
		assertTrue(cs2.get(0) instanceof ELMethodInvocation);
		assertEquals("a.b()", cs2.get(0).toString());
		assertTrue(cs2.get(2) instanceof ELPropertyInvocation);
		assertEquals("c", cs2.get(2).toString());

		assertTrue(cs.get(1) instanceof ELMethodInvocation);
		assertEquals("d", ((ELMethodInvocation)cs.get(1)).getName().getText());
		
	}

	public void testTagLibraryFunctionInvocation() {
		ELParser parser = ELParserUtil.getJbossFactory().createParser();
		String el = "#{e1 ? e2 : e3:f3() + (a:b(1,true) + c).d()}";
		ELModel model = parser.parse(el);
		
		List<ELInstance> instances = model.getInstances();
		
		assertEquals(1, instances.size());
		
		ELInstance instance = instances.get(0);
		ELExpression expr = instance.getExpression();
		List<ELInvocationExpression> is = expr.getInvocations();
		Set<String> keys = new HashSet<String>();
		for (ELInvocationExpression i: is) {
			String key = i.toString();
			keys.add(key);
		}
		assertTrue(keys.contains("e1"));
		assertTrue(keys.contains("e2"));
		assertTrue(keys.contains("e3:f3()"));
		assertTrue(keys.contains("a:b(1,true)"));
		assertTrue(keys.contains("c"));
		assertTrue(keys.contains(".d()"));
		assertEquals(6, is.size());
		
	}

	public void testArray() {
		ELParser parser = ELParserUtil.getJbossFactory().createParser();
		String el = "#{[a(),b(c)]}";
		ELModel model = parser.parse(el);
		
		List<ELInstance> instances = model.getInstances();
		
		assertEquals(1, instances.size());
		
		ELInstance instance = instances.get(0);
		ELExpression expr = instance.getExpression();
		List<ELInvocationExpression> is = expr.getInvocations();
		
		Set<String> keys = new HashSet<String>();
		for (ELInvocationExpression i: is) {
			String key = i.toString();
			keys.add(key);
		}
		assertTrue(keys.contains("a()"));
		assertTrue(keys.contains("b(c)"));
	}

	public void testFunctionCall() {
		doTestFunctionCall("#{q[p.n]}", 1, "p.n", "with member name");
		doTestFunctionCall("#{q[p.]}", 1, "p.", "before end of argument");
		doTestFunctionCall("#{q(p.:)}", 1, "p.", "before end of parameters");
		doTestFunctionCall("#{q(p.,s)}", 1, "p.", "before end of parameter (comma)");
		doTestFunctionCall("#{2*(2+p.)}", 1, "p.", "before end of expression");
		doTestFunctionCall("#{p.}", 1, "p.", "before end of EL");
		doTestFunctionCall("#{p.  #{a}", 2, "p.", "before start of another EL");

		doTestFunctionCall("#{q[p:n]}", 1, "p:n", "with member name");
		doTestFunctionCall("#{q[p:]}", 1, "p:", "before end of argument");
		doTestFunctionCall("#{q(p:)}", 1, "p:", "before end of parameters");
		doTestFunctionCall("#{q(p:,s)}", 1, "p:", "before end of parameter (comma)");
		doTestFunctionCall("#{2*(2+p:)}", 1, "p:", "before end of expression");
		doTestFunctionCall("#{p:}", 1, "p:", "before end of EL");
		doTestFunctionCall("#{p:  #{a}", 2, "p:", "before start of another EL");
	}

	void doTestFunctionCall(String el, int expectedInstancesCount, String expectedInvocation, String caseDescription) {
		ELParser parser = ELParserUtil.getJbossFactory().createParser();
		ELModel model = parser.parse(el);
		
		List<ELInstance> instances = model.getInstances();		
		assertEquals(expectedInstancesCount, instances.size());

		ELInstance instance = instances.get(0);
		ELExpression expr = instance.getExpression();
		List<ELInvocationExpression> invocations = expr.getInvocations();
		String message = "Member call is not detected " + caseDescription;
		assertFalse(message, invocations.isEmpty());
		for (ELInvocationExpression invocation: invocations) {
			if(expectedInvocation.equals(invocation.toString())) {
				return;
			}
		}
		fail(message);
	}

}
