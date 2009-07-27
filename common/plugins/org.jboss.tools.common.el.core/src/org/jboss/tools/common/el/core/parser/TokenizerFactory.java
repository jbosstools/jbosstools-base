/******************************************************************************* 
 * Copyright (c) 2007 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/ 
package org.jboss.tools.common.el.core.parser;

import java.util.List;
import java.util.regex.Pattern;

import org.jboss.tools.common.el.core.model.ELExpression;
import org.jboss.tools.common.el.core.model.ELInvocationExpression;
import org.jboss.tools.common.el.core.model.ELModel;
import org.jboss.tools.common.el.core.model.ELUtil;
import org.jboss.tools.common.el.internal.core.parser.rule.CallRule;
import org.jboss.tools.common.el.internal.core.parser.rule.ErrorRecoveryRule;
import org.jboss.tools.common.el.internal.core.parser.rule.ExpressionRule;
import org.jboss.tools.common.el.internal.core.parser.rule.OperationRule;
import org.jboss.tools.common.el.internal.core.parser.token.ArgEndTokenDescription;
import org.jboss.tools.common.el.internal.core.parser.token.ArgStartTokenDescription;
import org.jboss.tools.common.el.internal.core.parser.token.CommaTokenDescription;
import org.jboss.tools.common.el.internal.core.parser.token.DotTokenDescription;
import org.jboss.tools.common.el.internal.core.parser.token.EndELTokenDescription;
import org.jboss.tools.common.el.internal.core.parser.token.ExprEndTokenDescription;
import org.jboss.tools.common.el.internal.core.parser.token.ExprStartTokenDescription;
import org.jboss.tools.common.el.internal.core.parser.token.JavaNameTokenDescription;
import org.jboss.tools.common.el.internal.core.parser.token.OperationTokenDescription;
import org.jboss.tools.common.el.internal.core.parser.token.ParamEndTokenDescription;
import org.jboss.tools.common.el.internal.core.parser.token.ParamStartTokenDescription;
import org.jboss.tools.common.el.internal.core.parser.token.PrimitiveValueTokenDescription;
import org.jboss.tools.common.el.internal.core.parser.token.StartELTokenDescription;
import org.jboss.tools.common.el.internal.core.parser.token.StringTokenDescription;
import org.jboss.tools.common.el.internal.core.parser.token.UnaryTokenDescription;
import org.jboss.tools.common.el.internal.core.parser.token.WhiteSpaceTokenDescription;

/**
 * 
 * @author V. Kabanovich
 *
 */
public class TokenizerFactory {

	private static ITokenDescription[] DEFAULT_DESCRIPTION_SET = new ITokenDescription[] {
		ArgEndTokenDescription.INSTANCE,
		ArgStartTokenDescription.INSTANCE,
		DotTokenDescription.INSTANCE,
		EndELTokenDescription.INSTANCE,
		JavaNameTokenDescription.INSTANCE,
		OperationTokenDescription.INSTANCE,
		UnaryTokenDescription.INSTANCE,
		PrimitiveValueTokenDescription.INSTANCE,
		StartELTokenDescription.INSTANCE,
		StringTokenDescription.INSTANCE,
		WhiteSpaceTokenDescription.INSTANCE,			
	};

	private static IRule[] DEFAULT_RULE_SET = new IRule[] {
		ExpressionRule.INSTANCE,
		CallRule.INSTANCE,
		OperationRule.INSTANCE,
		ErrorRecoveryRule.INSTANCE,
	};

	public static Tokenizer createDefaultTokenizer() {
		Tokenizer t = new Tokenizer();
		t.setTokenDescriptions(DEFAULT_DESCRIPTION_SET);
		t.setRules(DEFAULT_RULE_SET);
		return t;
	}

	private static ITokenDescription[] JBOSS_DESCRIPTION_SET = new ITokenDescription[]{
		ArgEndTokenDescription.INSTANCE,
		ArgStartTokenDescription.INSTANCE,
		CommaTokenDescription.INSTANCE,
		DotTokenDescription.INSTANCE,
		EndELTokenDescription.INSTANCE,
		JavaNameTokenDescription.INSTANCE,
		OperationTokenDescription.INSTANCE,
		ParamEndTokenDescription.INSTANCE,
		ParamStartTokenDescription.INSTANCE,
		ExprStartTokenDescription.INSTANCE,
		ExprEndTokenDescription.INSTANCE,
		UnaryTokenDescription.INSTANCE,
		PrimitiveValueTokenDescription.INSTANCE,
		StartELTokenDescription.INSTANCE,
		StringTokenDescription.INSTANCE,
		WhiteSpaceTokenDescription.INSTANCE,			
	};

	private static IRule[] JBOSS_RULE_SET = new IRule[] {
		ExpressionRule.INSTANCE,
		CallRule.INSTANCE,
		OperationRule.INSTANCE,
		ErrorRecoveryRule.INSTANCE,
	};

	public static Tokenizer createJbossTokenizer() {
		Tokenizer t = new Tokenizer();
		t.setTokenDescriptions(JBOSS_DESCRIPTION_SET);
		t.setRules(JBOSS_RULE_SET);
		return t;
	}

	//Adds to default description set param start and param end to support access to collection and map elements.
	private static ITokenDescription[] COLLECTIONS_DESCRIPTION_SET = new ITokenDescription[] {
		ArgEndTokenDescription.INSTANCE,
		ArgStartTokenDescription.INSTANCE,
		DotTokenDescription.INSTANCE,
		EndELTokenDescription.INSTANCE,
		JavaNameTokenDescription.INSTANCE,
		OperationTokenDescription.INSTANCE,
		ParamEndTokenDescription.INSTANCE,
		ParamStartTokenDescription.INSTANCE,
		UnaryTokenDescription.INSTANCE,
		PrimitiveValueTokenDescription.INSTANCE,
		StartELTokenDescription.INSTANCE,
		StringTokenDescription.INSTANCE,
		WhiteSpaceTokenDescription.INSTANCE,			
	};

	public static Tokenizer createCollectionTokenizer() {
		Tokenizer t = new Tokenizer();
		t.setTokenDescriptions(COLLECTIONS_DESCRIPTION_SET);
		t.setRules(DEFAULT_RULE_SET);
		return t;
	}


	public static void main(String[] args) {
		boolean b = Pattern.matches("re(he|ma)*\\sat", "rehemahe at");
		System.out.println(b);
		if(true) return;
		String text = "ioioio#{a(1.2e1i) + b c + d}ioioio#{0}"; //$NON-NLS-1$
//"#{a[b()['l'].j]}";
//"#{g11.g12.y13} #{#{  #{a14.b15(x.t.u(uu.ii[9],  j)).b16(m17(v18(i19[2]).u20).)+ a21(c.).b.}";
//"#{not a.b(x,y) + s.h((6 != -8) & (7 + -iy88.g[9].h(7  div 8).i.j)+(8) ? 4 : 7,'p', a.b.c.d[null])}";
//"q82#{a(  g.h(7  +  8) + 8, g['h'].j(),'p')}k#{b}";
		Tokenizer t = createJbossTokenizer();
		LexicalToken token = t.parse(text);
		LexicalToken ti = token;
		
		while(ti != null) {
			int type = ti.getType();
			System.out.println(type + ":" + ti.getText() + ":"); //$NON-NLS-1$ //$NON-NLS-2$
			ti = ti.getNextToken();
		}
		List<SyntaxError> errors = t.getErrors();
		for (SyntaxError e: errors) {
			System.out.println("state=" + e.getState() + " position=" + e.getPosition() + " problem=" + e.getProblem()); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		}
		ELParser parser = ELParserUtil.getJbossFactory().createParser();
		ELModel model = parser.parse(text, 0, 90);
		System.out.println(model);

		ELExpression expr = model.getInstances().get(0).getExpression();
		System.out.println("Expression=" + expr); //$NON-NLS-1$
		List<ELInvocationExpression> is = expr.getInvocations();
		System.out.println("Invocations:"); //$NON-NLS-1$
		for (ELInvocationExpression i : is) {
			System.out.println(i);
		}

		int off = 8;
		ELExpression expr1 = ELUtil.findExpression(model, off);
		System.out.println("Expression at " + off + ": " + expr1); //$NON-NLS-1$ //$NON-NLS-2$
		
	}

}
