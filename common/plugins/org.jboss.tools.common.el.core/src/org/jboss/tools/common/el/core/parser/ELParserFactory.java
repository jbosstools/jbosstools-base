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
public class ELParserFactory {

	public static ELParser createDefaultParser() {
		return new ELParser() {
			ELParserImpl impl = new ELParserImpl();
			List<SyntaxError> errors = null;

			public ELModel parse(String source) {
				Tokenizer t = TokenizerFactory.createDefaultTokenizer();
				LexicalToken token = t.parse(source);
				errors = t.getErrors();
				if(token == null) {
					return null;
				}
				ELModelImpl model = impl.parse(token);
				model.setSource(source);
				model.setErrors(errors);
				return model;
			}

			public List<SyntaxError> getSyntaxErrors() {
				return errors;
			}
		};
	}

	public static ELParser createJbossParser() {
		return new ELParser() {
			ELParserImpl impl = new ELParserImpl();
			List<SyntaxError> errors = null;

			public ELModel parse(String source) {
				Tokenizer t = TokenizerFactory.createJbossTokenizer();
				LexicalToken token = t.parse(source);
				errors = t.getErrors();
				if(token == null) {
					return null;
				}
				ELModelImpl model = impl.parse(token);
				model.setSource(source);
				model.setErrors(errors);
				return model;
			}

			public List<SyntaxError> getSyntaxErrors() {
				return errors;
			}
		};
	}

}
