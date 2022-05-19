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

import org.jboss.tools.common.el.internal.core.parser.rule.CallRule;
import org.jboss.tools.common.el.internal.core.parser.rule.ErrorRecoveryRule;
import org.jboss.tools.common.el.internal.core.parser.rule.ExpressionRule;
import org.jboss.tools.common.el.internal.core.parser.rule.OperationRule;
import org.jboss.tools.common.el.internal.core.parser.token.ArgEndTokenDescription;
import org.jboss.tools.common.el.internal.core.parser.token.ArgStartTokenDescription;
import org.jboss.tools.common.el.internal.core.parser.token.ArrayEndTokenDescription;
import org.jboss.tools.common.el.internal.core.parser.token.ArrayStartTokenDescription;
import org.jboss.tools.common.el.internal.core.parser.token.AssignmentTokenDescription;
import org.jboss.tools.common.el.internal.core.parser.token.CommaTokenDescription;
import org.jboss.tools.common.el.internal.core.parser.token.DotTokenDescription;
import org.jboss.tools.common.el.internal.core.parser.token.EndELTokenDescription;
import org.jboss.tools.common.el.internal.core.parser.token.ExprEndTokenDescription;
import org.jboss.tools.common.el.internal.core.parser.token.ExprStartTokenDescription;
import org.jboss.tools.common.el.internal.core.parser.token.JavaNameTokenDescription;
import org.jboss.tools.common.el.internal.core.parser.token.LambdaTokenDescription;
import org.jboss.tools.common.el.internal.core.parser.token.OperationTokenDescription;
import org.jboss.tools.common.el.internal.core.parser.token.ParamEndTokenDescription;
import org.jboss.tools.common.el.internal.core.parser.token.ParamStartTokenDescription;
import org.jboss.tools.common.el.internal.core.parser.token.PrimitiveValueTokenDescription;
import org.jboss.tools.common.el.internal.core.parser.token.SemicolonTokenDescription;
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
		ArrayEndTokenDescription.INSTANCE,
		ArrayStartTokenDescription.INSTANCE,
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
		ArrayEndTokenDescription.INSTANCE,
		ArrayStartTokenDescription.INSTANCE,
		AssignmentTokenDescription.INSTANCE,	// + added to default
		CommaTokenDescription.INSTANCE,		// +
		DotTokenDescription.INSTANCE,
		EndELTokenDescription.INSTANCE,
		LambdaTokenDescription.INSTANCE,		// +
		JavaNameTokenDescription.INSTANCE,
		OperationTokenDescription.INSTANCE,
		ParamEndTokenDescription.INSTANCE,		// +
		ParamStartTokenDescription.INSTANCE,	// +
		ExprStartTokenDescription.INSTANCE,		// +
		ExprEndTokenDescription.INSTANCE,		// +
		UnaryTokenDescription.INSTANCE,
		PrimitiveValueTokenDescription.INSTANCE,
		SemicolonTokenDescription.INSTANCE,		// +
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

}
