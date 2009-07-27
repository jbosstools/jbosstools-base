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
import java.util.Stack;

import org.jboss.tools.common.el.core.model.ELModel;
import org.jboss.tools.common.el.internal.core.model.ELModelImpl;
import org.jboss.tools.common.el.internal.core.parser.ELParserImpl;

/**
 * 
 * @author V. Kabanovich
 *
 */
public class ELParserUtil {

	private static ELParserFactory DEFAULT_FACTORY = new DefaultFactory() {
		public ELParser newParser() {
			return new DefaultParser() {
				protected Tokenizer createTokenizer() {
					return TokenizerFactory.createDefaultTokenizer();
				}
				public void dispose() {
					super.dispose();
					release(this);
				}
			};
		}
	};

	public static ELParserFactory getDefaultFactory() {
		return DEFAULT_FACTORY;
	}

	private static ELParserFactory COLLECTION_FACTORY = new DefaultFactory() {
		public ELParser newParser() {
			return new DefaultParser() {
				protected Tokenizer createTokenizer() {
					return TokenizerFactory.createCollectionTokenizer();
				}
				public void dispose() {
					super.dispose();
					release(this);
				}
			};
		}
	};

	/**
	 * Extends default factory by ability to parse method invocation without parameters
	 * to support access to collection and map elements by .iterator().next()
	 * @return
	 */
	public static ELParserFactory getCollectionFactory() {
		return COLLECTION_FACTORY;
	}

	private static ELParserFactory JBOSS_FACTORY = new DefaultFactory() {
		public ELParser newParser() {
			return new DefaultParser() {
				protected Tokenizer createTokenizer() {
					return TokenizerFactory.createJbossTokenizer();
				}
				public void dispose() {
					super.dispose();
					release(this);
				}
			};
		}
	};

	public static ELParserFactory getJbossFactory() {
		return JBOSS_FACTORY;
	}

	private static abstract class DefaultFactory implements ELParserFactory {
		protected Stack<ELParser> inUse = new Stack<ELParser>();
		protected Stack<ELParser> free = new Stack<ELParser>();

		public ELParser createParser() {
			synchronized(this) {
				if(!free.isEmpty()) {
					//reuse
					ELParser parser = free.pop();
					inUse.push(parser);
					return parser;
				}
			}
			ELParser parser = newParser();
			synchronized(this) {
				//new
				inUse.push(parser);
			}			
			return parser;
		}

		protected abstract ELParser newParser();

		public void release(ELParser parser) {
			synchronized(this) {
				//release
				boolean b = inUse.remove(parser);
				if(!b) return;
				free.push(parser);
			}
		}
	}

	private static abstract class DefaultParser implements ELParser {
		ELParserImpl impl = new ELParserImpl();
		List<SyntaxError> errors = null;
		Tokenizer t = createTokenizer();

		public ELModel parse(String source) {
			return parse(source, 0, source.length());
		}

		public ELModel parse(String source, int start, int length) {
			try {
				LexicalToken token = t.parse(source, start, length);
				errors = t.getErrors();
				ELModelImpl model = impl.parse(token);
				model.setSource(source);
				model.setErrors(errors);
				return model;
			} finally {
				t.dispose();
				dispose();
			}
		}

		protected abstract Tokenizer createTokenizer();

		public void dispose() {
			errors = null;
		}

	}

}
