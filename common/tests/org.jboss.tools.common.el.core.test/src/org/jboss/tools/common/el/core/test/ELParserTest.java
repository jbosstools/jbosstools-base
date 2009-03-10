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
import java.util.Random;

import org.jboss.tools.common.el.core.model.ELInstance;
import org.jboss.tools.common.el.core.model.ELModel;
import org.jboss.tools.common.el.core.parser.ELParser;
import org.jboss.tools.common.el.core.parser.ELParserFactory;
import org.jboss.tools.common.el.core.parser.ELParserUtil;
import org.jboss.tools.common.el.core.parser.LexicalToken;
import org.jboss.tools.common.el.core.parser.SyntaxError;
import org.jboss.tools.common.el.core.parser.Tokenizer;
import org.jboss.tools.common.el.core.parser.TokenizerFactory;

import junit.framework.TestCase;

public class ELParserTest extends TestCase {

	public ELParserTest() {
	}

	protected void setUp() throws Exception {
	}

	public void testTokenizerOnCorrectEL() {
		Tokenizer t = TokenizerFactory.createJbossTokenizer();
		// 1. One variable
		checkCorrectEL(t, "#{a}");
		// 2. Two EL instances
		checkCorrectEL(t, "#{a}#{b}");
		// 3. Property invocation
		checkCorrectEL(t, "#{a.b}");
		// 4. Argument invocation
		checkCorrectEL(t, "#{a.b['xxx']}");
		// 5a. Method invocation
		checkCorrectEL(t, "#{a.b()}");
		// 5b. Method invocation with one parameter
		checkCorrectEL(t, "#{a.b(c)}");
		// 5b. Method invocation with two parameters
		checkCorrectEL(t, "#{a.b(c.d , e['u'])}");
		// 6. Numeric
		checkCorrectEL(t, "#{a.b(16.900)}");
		// 7. Boolean
		checkCorrectEL(t, "#{a.b(false)}");
		// 8. Operators
		checkCorrectEL(t, "#{a.b(7 + 8) * 4 / 2 - 1}");
		// 9. Complex expressions
		checkCorrectEL(t, "#{a.b(7 + 8) * (4 / 2 - 1)/c.d}");
		// 10. Complex expressions
		checkCorrectEL(t, "#{a.b(7 + 8) * (4 / 2 - 1)/c.d}");
	}

	/**
	 * JBIDE-3132 An OutOfMemory exception is thrown by Code assist for
	 * "#{messages[ "
	 */
	public void testJBIDE3132() {
		ELParser parser = ELParserUtil.getJbossFactory().createParser();
		String el = "#{messages[";
		ELModel model = parser.parse(el);
		List<ELInstance> is = model.getInstances();
		assertEquals(1, is.size());
		assertEquals(el, is.get(0).getText());
	}

	public void testElEmptyOperator() {
		Tokenizer t = TokenizerFactory.createJbossTokenizer();
		checkCorrectEL(t, "#{empty a}");
	}

	public void testElLogicalNotOperators() {
		Tokenizer t = TokenizerFactory.createJbossTokenizer();
		checkCorrectEL(t, "#{not a == null}");
		checkCorrectEL(t, "#{!a eq null}");
	}

	public void testElLogicalAndOperators() {
		Tokenizer t = TokenizerFactory.createJbossTokenizer();
		checkCorrectEL(t, "#{a!=null and a!=1}");
		checkCorrectEL(t, "#{a!=null && a!=1}");
	}

	public void testElLogicalOrOperators() {
		Tokenizer t = TokenizerFactory.createJbossTokenizer();
		checkCorrectEL(t, "#{a!=null or a!=1}");
		checkCorrectEL(t, "#{a!=null || a!=1}");
	}

	public void testElConditionalOperator() {
		Tokenizer t = TokenizerFactory.createJbossTokenizer();
		checkCorrectEL(t, "#{a?1:2}");
	}

	public void testElRelationalOperator() {
		Tokenizer t = TokenizerFactory.createJbossTokenizer();
		checkCorrectEL(t, "#{a == b}");
		checkCorrectEL(t, "#{a eq b}");
		checkCorrectEL(t, "#{a != b}");
		checkCorrectEL(t, "#{a ne b}");
		checkCorrectEL(t, "#{a < b}");
		checkCorrectEL(t, "#{a lt b}");
		checkCorrectEL(t, "#{a > b}");
		checkCorrectEL(t, "#{a < b}");
		checkCorrectEL(t, "#{a <= b}");
		checkCorrectEL(t, "#{a ge b}");
		checkCorrectEL(t, "#{a >= b}");
		checkCorrectEL(t, "#{a le b}");
		checkCorrectEL(t, "#{'a' < 'b'}");

	}

	public void testElArithmeticOperators() {
		Tokenizer t = TokenizerFactory.createJbossTokenizer();
		checkCorrectEL(t, "#{a*b}");
		checkCorrectEL(t, "#{a/b}");
		checkCorrectEL(t, "#{a div b}");
		checkCorrectEL(t, "#{a%b}");
		checkCorrectEL(t, "#{a mod b}");
		checkCorrectEL(t, "#{a-b}");
		checkCorrectEL(t, "#{a+b}");
		checkCorrectEL(t, "#{-b+1}");
		checkCorrectEL(t, "#{-b}");
	}

	public void testElLiteralExpressions() {
		Tokenizer t = TokenizerFactory.createJbossTokenizer();
		checkCorrectEL(t, "#{\"#{\"}");
		checkCorrectEL(t, "\\#{exprA}");
		checkCorrectEL(t, "#{\"\\\"exprA\\\"\"}");
		checkCorrectEL(t, "#{\"\\\"#\\\"\"}");
		checkCorrectEL(t,"#{'+'}");
		// Ask google :)
		// http://java.sun.com/javaee/5/docs/tutorial/doc/bnahq.html#indexterm-357
		checkCorrectEL(t,"#{'#{'}exprB}"); //Why this is correct?
	}

	public void testElReferencesObjectProperties() {
		Tokenizer t = TokenizerFactory.createJbossTokenizer();
		checkCorrectEL(t, "#{customer.address[\"street\"]}");
		checkCorrectEL(t, "#{customer.address['street']}");
		checkCorrectEL(t, "#{customer.address[1]}");
		checkCorrectEL(t, "#{customer.address[customer.address[customer.address[0]]]}");
		checkCorrectEL(t, "#{planets[object.counter].mass}");
	}

	public void testElSimpleTypes() {
		Tokenizer t = TokenizerFactory.createJbossTokenizer();
		checkCorrectEL(t, "#{\"string\"}");
		checkCorrectEL(t, "#{'string'}");
		checkCorrectEL(t, "#{11.0}");
		checkCorrectEL(t, "#{1.2E4}");
		checkCorrectEL(t, "#{null}");
		checkCorrectEL(t, "#{1}");
		checkCorrectEL(t, "#{true}");
		checkCorrectEL(t, "#{false}");
	}

	public void testReservedWordAnd() {
		Tokenizer t = TokenizerFactory.createJbossTokenizer();
		doReservedWordTest(t,"and");
	}
	
	public void testReservedWordOr() {
		Tokenizer t = TokenizerFactory.createJbossTokenizer();
		doReservedWordTest(t,"or");
	}
	
	public void testReservedWordNot() {
		Tokenizer t = TokenizerFactory.createJbossTokenizer();
		checkIncorrectEL(t, "#{"+"not"+"}",5);
		checkIncorrectEL(t, "#{"+"not"+".method1}", 5);
		checkIncorrectEL(t, "#{"+"not"+"*"+"not"+"}", 5);
	}		
	
	public void testReservedWordEq() {
		Tokenizer t = TokenizerFactory.createJbossTokenizer();
		doReservedWordTest(t,"eq");
	}
	
	public void testReservedWordNe() {
		Tokenizer t = TokenizerFactory.createJbossTokenizer();
		doReservedWordTest(t,"ne");
	}
	public void testReservedWordLt() {
		Tokenizer t = TokenizerFactory.createJbossTokenizer();
		doReservedWordTest(t,"lt");
	}	
	
	public void testReservedWordGt() {
		Tokenizer t = TokenizerFactory.createJbossTokenizer();
		doReservedWordTest(t,"gt");
	}
	
	public void testReservedWordLe() {
		Tokenizer t = TokenizerFactory.createJbossTokenizer();
		doReservedWordTest(t,"le");
	}
	
	public void testReservedWordGe() {
		Tokenizer t = TokenizerFactory.createJbossTokenizer();
		doReservedWordTest(t,"ge");
	}
	
	public void testReservedWordTrue() {
		Tokenizer t = TokenizerFactory.createJbossTokenizer();
		checkIncorrectEL(t, "#{true.method1}", 6);
	}
	
	public void testReservedWordFalse() {
		Tokenizer t = TokenizerFactory.createJbossTokenizer();
		checkIncorrectEL(t, "#{false.method1}", 7);
	}
	
	public void testReservedWordNull() {
		Tokenizer t = TokenizerFactory.createJbossTokenizer();
		checkIncorrectEL(t, "#{null.method1}", 6);
	}
	
	public void testReservedWordInstanceof() {
		Tokenizer t = TokenizerFactory.createJbossTokenizer();
		doReservedWordTest(t,"instanceof");
	}
	
	public void testReservedWordEmpty() {
		Tokenizer t = TokenizerFactory.createJbossTokenizer();
		checkIncorrectEL(t, "#{empty.method1}", 7);
	}
	
	public void testReservedWordDiv() {
		Tokenizer t = TokenizerFactory.createJbossTokenizer();
		doReservedWordTest(t,"div");
	}
	
	public void testReservedWordMod() {
		Tokenizer t = TokenizerFactory.createJbossTokenizer();
		doReservedWordTest(t,"mod");
	}
	
	private void doReservedWordTest(Tokenizer t, String keyword) {
		checkIncorrectEL(t, "#{"+keyword+"}",2);
		checkIncorrectEL(t, "#{"+keyword+".method1}", 2);
		checkIncorrectEL(t, "#{"+keyword+"+"+keyword+"}", 2);
	}
	
	public void testComplexMath() {
		Tokenizer t = TokenizerFactory.createJbossTokenizer();
		checkCorrectEL(t,
				"#{(7 * (13 + 7.9)) * (a + b.c / d) / (1.3E5) - (1/a.b+8./c.d)}");
	}

	public void testComplexInvocation() {
		Tokenizer t = TokenizerFactory.createJbossTokenizer();
		checkCorrectEL(t,
				"#{a.b[a1.b1(a2.b2,a3.b3(x))][y].c(a4.b4,a5.b5[a6(b6)])}");
	}

	public void testSeveralELInstances() {
		Tokenizer t = TokenizerFactory.createJbossTokenizer();
		checkCorrectEL(t, "aaa#{a}bbb#{1}#{c()}");
	}

	private void checkCorrectEL(Tokenizer t, String test) {
		LexicalToken token = t.parse(test);
		assertEquals(test, restore(token));
		List<SyntaxError> errors = t.getErrors();
		assertEquals("EL '" + test + "' has no syntax problems.", 0, errors
				.size());
		System.out.println("Passed correct EL '" + test + "'");
	}

	public void testTokenizerOnIncorrectEL() {
		Tokenizer t = TokenizerFactory.createJbossTokenizer();
		// 1. Dot unfollowed by name
		checkIncorrectEL(t, "#{a.}", 4);
		// 2. Incorrect use of ')'
		checkIncorrectEL(t, "#{a.b + -c.d + g)}", 16);
		// 2. Incorrect use of '.' in second EL instance
		checkIncorrectEL(t, "#{a.b + -c.d + g}#{hh.vv..m()}", 25);
		// incorrect operation
		checkIncorrectEL(t, "#{!}", 3);
		checkIncorrectEL(t, "#{+}", 3);
		checkIncorrectEL(t, "#{-}", 3);
		checkIncorrectEL(t, "#{()}", 3);
		checkIncorrectEL(t, "#{[1]}", 2);
		checkIncorrectEL(t, "#{%}", 2);
		checkIncorrectEL(t, "#{:}", 2);
		checkIncorrectEL(t, "#{/}", 2);
		checkIncorrectEL(t, "#{*}", 2);
		checkIncorrectEL(t, "#{&&}", 2);
		checkIncorrectEL(t, "#{||}", 2);
		checkIncorrectEL(t, "#{<}", 2);
		checkIncorrectEL(t, "#{>}", 2);
		checkIncorrectEL(t, "#{<=}", 2);
		checkIncorrectEL(t, "#{>=}", 2);
		checkIncorrectEL(t, "#{==}", 2);
		checkIncorrectEL(t, "#{!=}", 3);
		checkIncorrectEL(t, "#{?}", 2);
		checkIncorrectEL(t, "#{=}", 2);

		checkIncorrectEL(t, "#{c.a[1.5E7]}",7); //TODO
	}

	public void testLexemas() {
		Tokenizer t = TokenizerFactory.createJbossTokenizer();
		checkIncorrectEL(t, "#{#identifier}", 2);
		checkIncorrectEL(t, "#{2identifier}", 3);
		checkIncorrectEL(t, "#{$identifier}", 2);
		checkIncorrectEL(t, "#{i1.2methodorproperty}", 5);
	}
	
	private void checkIncorrectEL(Tokenizer t, String test,
			int expectedErrorPosition) {
		LexicalToken token = t.parse(test);
		List<SyntaxError> errors = t.getErrors();
		assertTrue("EL '" + test + "' has no syntax problems. ", errors.size() > 0);
		assertEquals(expectedErrorPosition, errors.get(0).getPosition());
		String correctPart = test.substring(0, expectedErrorPosition);
		String parsed = restore(token);
		assertTrue(
				"Parsed value should be identical to source at least until first problem.",
				parsed.startsWith(correctPart));
		System.out.println("Passed incorrect EL '" + test + "'");
	}

	private String restore(LexicalToken token) {
		StringBuffer sb = new StringBuffer();
		while (token != null) {
			sb.append(token.getText());
			token = token.getNextToken();
		}
		return sb.toString();
	}

	static int TREAD_NUMBER = 20;
	static int CALL_NUMBER = 1000;

	public void testMultiThreadAccess() {

		final ELParserFactory factory = ELParserUtil.getJbossFactory();
		final Random random = new Random();
		class Z {
			int counter = 0;
			int parserErrors = 0;
			int syntaxErrors = 0;
			int expectedSyntaxErrors = 0;

			synchronized void addParserError() {
				parserErrors++;
			}

			synchronized void addSyntaxError() {
				syntaxErrors++;
			}

			synchronized void addExpectedSyntaxError() {
				expectedSyntaxErrors++;
			}
		}
		final Z z = new Z();

		for (int i = 0; i < TREAD_NUMBER; i++) {
			Runnable r = new Runnable() {
				public void run() {
					z.counter++;
					try {
						for (int j = 0; j < CALL_NUMBER; j++) {
							boolean addError = random.nextInt(100) < 50;
							if (addError)
								z.addExpectedSyntaxError();
							ELParser parser = factory.createParser();
							String el = "#{(a + b(c.d" + random.nextInt(1000)
									+ ") + c().k" + (addError ? "." : "")
									+ ") + 9.7}";
							ELModel model = parser.parse(el);
							LexicalToken t = model.getFirstToken();
							if (!el.equals(restore(t))) {
								z.addParserError();
							}
							if (model.getSyntaxErrors() != null
									&& model.getSyntaxErrors().size() > 0) {
								z.addSyntaxError();
							}
						}
					} finally {
						z.counter--;
					}
				}
			};
			new Thread(r).start();
		}

		while (z.counter > 0) {
			try {
				Thread.sleep(100);
			} catch (InterruptedException e) {

			}
		}

		System.out.println("testMultiThreadAccess: Expected syntax errors="
				+ z.expectedSyntaxErrors);

		assertEquals(0, z.parserErrors);

		assertEquals(z.expectedSyntaxErrors, z.syntaxErrors);

	}

}
