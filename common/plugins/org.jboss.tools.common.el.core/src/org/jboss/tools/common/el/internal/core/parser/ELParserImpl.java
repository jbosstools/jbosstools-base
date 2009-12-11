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
package org.jboss.tools.common.el.internal.core.parser;

import org.jboss.tools.common.el.core.parser.LexicalToken;
import org.jboss.tools.common.el.core.parser.Tokenizer;
import org.jboss.tools.common.el.internal.core.model.ELArgumentImpl;
import org.jboss.tools.common.el.internal.core.model.ELArgumentExpressionImpl;
import org.jboss.tools.common.el.internal.core.model.ELComplexExpressionImpl;
import org.jboss.tools.common.el.internal.core.model.ELExpressionImpl;
import org.jboss.tools.common.el.internal.core.model.ELInstanceImpl;
import org.jboss.tools.common.el.internal.core.model.ELInvocationExpressionImpl;
import org.jboss.tools.common.el.internal.core.model.ELMethodInvocationImpl;
import org.jboss.tools.common.el.internal.core.model.ELModelImpl;
import org.jboss.tools.common.el.internal.core.model.ELMultiExpressionImpl;
import org.jboss.tools.common.el.internal.core.model.ELOperatorImpl;
import org.jboss.tools.common.el.internal.core.model.ELParametersImpl;
import org.jboss.tools.common.el.internal.core.model.ELPropertyInvocationImpl;
import org.jboss.tools.common.el.internal.core.model.ELValueExpressionImpl;
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
public class ELParserImpl {
	LexicalToken current;

	public ELModelImpl parse(LexicalToken start) {
		if(current != null) {
			throw new RuntimeException("Cannot reuse parser while it is running."); //$NON-NLS-1$
		}
		try {
			ELModelImpl model = new ELModelImpl();
			model.setFirstToken(start);
			current = start;
			while (current != null) {
				if (current.getType() == StartELTokenDescription.START_EL) {
					ELInstanceImpl instance = readELInstance();
					if (instance != null) {
						model.addInstance(instance);
					}
				} else if (!hasNextToken()) {
					break;
				} else {
					if (lookUpNextToken(current) == null) {
						break;
					}
					setNextToken();
				}
			}
			return model;
		} finally {
			current = null;
		}
	}

	protected ELInstanceImpl readELInstance() {
		if(current == null || current.getType() != StartELTokenDescription.START_EL) {
			return null;
		}
		ELInstanceImpl instance = new ELInstanceImpl();
		instance.setFirstToken(current);
		ELExpressionImpl expression = null;
//		setNextToken();
		if(lookUpNextToken(current) != null) {
			setNextToken();
			expression = readExpression();
		} else {
			current = current.getNextToken();
		}
		if(expression == null) {
			//create fake invocation expression
			expression = new ELPropertyInvocationImpl();
			int p = current != null ? current.getStart() : instance.getEndPosition();
			LexicalToken t = new LexicalToken(p, 0, "", JavaNameTokenDescription.JAVA_NAME); //$NON-NLS-1$
			expression.setFirstToken(t);
			expression.setLastToken(t);
		}
		if(expression != null) {
			instance.setExpression(expression);
			instance.setLastToken(expression.getLastToken());
		}
		do {
			if(current == null) {
				if(instance.getLastToken() == null) {
					instance.setLastToken(instance.getFirstToken());
				}
				return instance;
			} else if(current.getType() == StartELTokenDescription.START_EL) {
				instance.setLastToken(current.getPreviousToken());
				return instance;
			} else if(current.getType() == EndELTokenDescription.END_EL) {
				instance.setLastToken(current);
				setNextToken();
				return instance;
			} else if(!hasNextToken()) {
				instance.setLastToken(current);
				return instance;
			}
			setNextToken();
		} while(true);
	}

	protected ELExpressionImpl readExpression() {
		if(current == null) return null;
		ELExpressionImpl single = readSingleExpression();
		if(single == null) return null;
		if(current == null || current.getType() != OperationTokenDescription.OPERATION) {
			return single;
		}
		ELMultiExpressionImpl multi = new ELMultiExpressionImpl();
		multi.setFirstToken(single.getFirstToken());
		multi.addExpression(single);		
		while(current != null && current.getType() == OperationTokenDescription.OPERATION && hasNextToken()) {
			ELOperatorImpl operator = new ELOperatorImpl();
			operator.setFirstToken(current);
			operator.setLastToken(current);
			multi.addOperator(operator);
			multi.setLastToken(operator.getLastToken());
			setNextToken();
			single = readSingleExpression();
			if(single != null) {
				multi.addExpression(single);
				multi.setLastToken(single.getLastToken());
			}
		}
		return multi;
	}

	protected ELExpressionImpl readSingleExpression() {
		if(current == null) return null;
		switch(current.getType()) {
			case ExprStartTokenDescription.EXPR_START:
			case UnaryTokenDescription.UNARY:
				return readComplexExpression();
			case StringTokenDescription.STRING:
				LexicalToken f = lookUpNextToken(current);
				if(f != null && f.getType() == DotTokenDescription.DOT) {
					return readInvocationExpression();
				}
			case PrimitiveValueTokenDescription.PRIMITIVE_VALUE:
				ELExpressionImpl expr = new ELValueExpressionImpl();
				expr.setFirstToken(current);
				expr.setLastToken(current);
				setNextToken();
				return expr;
			case JavaNameTokenDescription.JAVA_NAME:
				return readInvocationExpression();
		}
		return null;
	}

	protected ELExpressionImpl readComplexExpression() {
		ELComplexExpressionImpl expr = new ELComplexExpressionImpl();
		expr.setFirstToken(current);
		if(!hasNextToken()) {
			expr.setLastToken(current);
			return expr;
		}
		setNextToken();
		ELExpressionImpl child = readExpression();
		if(child != null) {
			expr.setExpression(child);
			expr.setLastToken(child.getLastToken());
		}
		if(current != null && current.getType() == ExprEndTokenDescription.EXPR_END) {
			expr.setLastToken(current);
			setNextToken();
		}
		return expr;
	}

	protected ELInvocationExpressionImpl readInvocationExpression() {
		if(current == null || 
			(current.getType() != JavaNameTokenDescription.JAVA_NAME &&
			 current.getType() != StringTokenDescription.STRING)) {
			return null;
		}
		ELPropertyInvocationImpl name = new ELPropertyInvocationImpl();
		name.setName(current);
		ELInvocationExpressionImpl result = name;
		setNextToken();
		if(current != null) switch (current.getType()) {
			case ParamStartTokenDescription.PARAM_START:
				ELParametersImpl params = readParameters();
				ELMethodInvocationImpl method = new ELMethodInvocationImpl();
				method.setName(name.getName());
				method.setParameters(params);
				result = method;
				//do not break we are ready to have [] suffix here
			case ArgStartTokenDescription.ARG_START:
				while(current != null && current.getType() == ArgStartTokenDescription.ARG_START) {
					ELArgumentImpl arg = readArgument();
					ELArgumentExpressionImpl call = new ELArgumentExpressionImpl();
					call.setArgument(arg);
					call.setLeft(result);
					result = call;
				}
				break;
		}
		if(current != null && current.getType() == DotTokenDescription.DOT) {
			LexicalToken dot = current;
			setNextToken();
			ELInvocationExpressionImpl right = readInvocationExpression();
			if(right != null) {
				ELInvocationExpressionImpl r = right;
				while(r.getLeft() != null) r = r.getLeft();
				if(r instanceof ELPropertyInvocationImpl) {
					((ELPropertyInvocationImpl)r).setSeparator(dot);
				} else {
					//is it possible?
				}
				r.setLeft(result);				
				result = right;
			} else {
				ELPropertyInvocationImpl incompleteProperty = new ELPropertyInvocationImpl();
				incompleteProperty.setSeparator(dot);
				incompleteProperty.setLastToken(dot);
				incompleteProperty.setLeft(result);
				result = incompleteProperty;
			}
		}
		return result;
	}

	protected ELParametersImpl readParameters() {
		ELParametersImpl parameters = new ELParametersImpl();
		parameters.setFirstToken(current);
		if(!hasNextToken()) {
			parameters.setLastToken(current);
			return parameters;
		}
		setNextToken();
		ELExpressionImpl expression = readExpression();
		if(expression != null) {
			parameters.addParameter(expression);
			parameters.setLastToken(expression.getLastToken());
		}
		while(current != null && current.getType() == CommaTokenDescription.COMMA) {
			if(!hasNextToken()) {
				parameters.setLastToken(current);
				return parameters;
			}
			setNextToken();
			expression = readExpression();
			if(expression != null) {
				parameters.addParameter(expression);
				parameters.setLastToken(expression.getLastToken());
			}
		}
		if(current != null && current.getType() == ParamEndTokenDescription.PARAM_END) {
			parameters.setLastToken(current);
			setNextToken();
		}
		
		return parameters;
	}

	protected ELArgumentImpl readArgument() {
		ELArgumentImpl arg = new ELArgumentImpl();
		arg.setFirstToken(current);
		arg.setLastToken(current);
		if(!hasNextToken()) {
			setNextToken();
			return arg;
		}
		setNextToken();
		ELExpressionImpl expr = readExpression();
		if(expr != null) {
			arg.setArgument(expr);
			arg.setLastToken(expr.getLastToken());
		}
		if(current != null && current.getType() == ArgEndTokenDescription.ARG_END) {
			arg.setLastToken(current);
			setNextToken();
		}
		return arg;
	}

	private boolean hasNextToken() {
		return lookUpNextToken(current) != null;
	}

	private LexicalToken lookUpNextToken(LexicalToken token) {
		LexicalToken c = token;
		while(c != null 
				&& (c == token 
					|| c.getType() == WhiteSpaceTokenDescription.WHITESPACE
					|| c.getType() == Tokenizer.LITERAL)) {
			c = c.getNextToken();
		}
		return c;
	}

	private void setNextToken() {
		current = lookUpNextToken(current);
	}

}
