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

import org.jboss.tools.common.el.core.model.ELModel;
import org.jboss.tools.common.el.internal.core.model.ELModelImpl;
import org.jboss.tools.common.el.internal.core.parser.ELParserImpl;

/**
 * 
 * @author V. Kabanovich
 *
 */
public class ELParserUtil {

	public static ELParserFactory getDefaultFactory() {
		return new ELParserFactory() {
			public ELParser createParser() {
				return new DefaultParser() {
					protected Tokenizer createTokenizer() {
						return TokenizerFactory.createDefaultTokenizer();
					}
				};
			}
		};
	}

	public static ELParserFactory getJbossFactory() {
		return new ELParserFactory() {
			public ELParser createParser() {
				return new DefaultParser() {
					protected Tokenizer createTokenizer() {
						return TokenizerFactory.createJbossTokenizer();
					}
				};
			}
		};
	}

	private static abstract class DefaultParser implements ELParser {
		ELParserImpl impl = new ELParserImpl();
		List<SyntaxError> errors = null;

		public ELModel parse(String source) {
			return parse(source, 0, source.length());
		}

		public ELModel parse(String source, int start, int length) {
			Tokenizer t = createTokenizer();
			LexicalToken token = t.parse(source, start, length);
			errors = t.getErrors();
			ELModelImpl model = impl.parse(token);
			model.setSource(source);
			model.setErrors(errors);
			return model;
		}

		public List<SyntaxError> getSyntaxErrors() {
			return errors;
		}

		protected abstract Tokenizer createTokenizer();
	}

}
