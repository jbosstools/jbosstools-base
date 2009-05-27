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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import org.jboss.tools.common.el.internal.core.parser.rule.BasicStates;

/**
 * 
 * @author V. Kabanovich
 *
 */
public class Tokenizer {
	public static int LITERAL = -10;
	
	private Map<Integer, ITokenDescription> tokenDescriptions = new HashMap<Integer, ITokenDescription>();
	private Map<Integer, List<IRule>> rules = new HashMap<Integer, List<IRule>>();

	private String sourceString;
	private int index = 0;

	private LexicalToken start;
	private LexicalToken last;
	
	private int state;
	private Properties context = new Properties();

	List<SyntaxError> errors = new ArrayList<SyntaxError>();

	public Tokenizer() {}

	/**
	 * Prepares this tokenizer to be reused with the same token descriptions and rules.
	 * 
	 */
	public void dispose() {
		sourceString = null;
		index = 0;
		start = null;
		last = null;
		context.clear();
		errors.clear();
	}

	public void setTokenDescriptions(ITokenDescription[] ds) {
		tokenDescriptions.clear();
		for (int i = 0; i < ds.length; i++) {
			int type = ds[i].getType();
			if(tokenDescriptions.containsKey(type)) {
				throw new IllegalArgumentException("Token type " + type + " is not unique."); //$NON-NLS-1$ //$NON-NLS-2$
			}
			tokenDescriptions.put(type, ds[i]);
		}
	}

	public ITokenDescription getTokenDescription(int id) {
		return tokenDescriptions.get(id);
	}

	public void setRules(IRule[] rules) {
		this.rules.clear();
		for (int i = 0; i < rules.length; i++) {
			int[] ss = rules[i].getStartStates();
			for (int j = 0; j < ss.length; j++) {
				List<IRule> rs = this.rules.get(ss[j]);
				if(rs == null) {
					rs = new ArrayList<IRule>();
					this.rules.put(ss[j], rs);
				}
				rs.add(rules[i]);
			}
		}
	}

	public LexicalToken parse(String sourceString) {
		return parse(sourceString, 0, sourceString.length());
	}

	public LexicalToken parse(String sourceString, int initialOffset, int length) {
		this.sourceString = sourceString;
		errors.clear();
		index = initialOffset;
		start = new LexicalToken(initialOffset, 0, "", -1000); //$NON-NLS-1$
		last = start;
		state = BasicStates.STATE_EXPECTING_EL;

		int lastIndex = initialOffset + length;
		if(lastIndex > sourceString.length()) lastIndex = sourceString.length();
		while(index < lastIndex) {
			boolean done = false;
			List<IRule> rs = rules.get(state);
			for (IRule rule : rs) {
				int[] ts = rule.getTokenTypes(state);
				for (int j = 0; !done && j < ts.length; j++) {
					ITokenDescription td = tokenDescriptions.get(ts[j]);
					if (td != null && td.isStart(this, index)) {
						td.read(this, index);
						state = rule.getFinalState(state, ts[j]);
						done = true;
					}
				}
			}
			if(!done) {
				if(state == BasicStates.STATE_EXPECTING_EL || state == BasicStates.STATE_ERROR) {
					char ch = readNextChar();
					if(ch == '\0') break;
				} else {
					SyntaxError error = new SyntaxError(index, state);
					String problem = null;
					for (IRule rule : rs) {
						problem = rule.getProblem(state, this);
						if(problem != null) break;
					}
					error.setProblem(problem);
					errors.add(error);
					state = BasicStates.STATE_ERROR;
				}
			}
		}
		LexicalToken result = start.getNextToken();
		if(result != null) {
			result.makeItFirst();
		}
		if(last != null && last.getStart() + last.getLength() < sourceString.length()
			&& last.getType() != LITERAL) {
			int lastEnd = last.getStart() + last.getLength();
			LexicalToken t = new LexicalToken(lastEnd, length, getCharSequence(lastEnd, sourceString.length()), LITERAL);
			last.setNextToken(t);
			last = t;
		}
		return result;
	}

	private static List<SyntaxError> EMPTY = new ArrayList<SyntaxError>();

	/**
	 * Copies errors in order to reuse this object.
	 * @return
	 */
	public List<SyntaxError> getErrors() {
		if(errors.size() == 0) return EMPTY;
		List<SyntaxError> copy = new ArrayList<SyntaxError>();
		copy.addAll(errors);
		return copy;
	}

	public void addSyntaxError(SyntaxError error) {
		errors.add(error);
	}

	public void addToken(int type, int start, int end) {
		if(end < 0) return;
		int lastEnd = last.getStart() + last.getLength();
		if(start > lastEnd) {
			int length = start - lastEnd;
			LexicalToken t = new LexicalToken(lastEnd, length, getCharSequence(lastEnd, start), LITERAL);
			last.setNextToken(t);
			last = t;
			index = start;
		}
		LexicalToken t = new LexicalToken(start, end - start, getCharSequence(start, end), type);
		last.setNextToken(t);
		last = t;
		index = end;
	}

	public Properties getContext() {
		return context;
	}

	private CharSequence getCharSequence(int start, int end) {
		String text = sourceString.substring(start, end);
		return text.subSequence(0, text.length());
	}

	public char readNextChar() {
		char c = '\0';
		if (index < sourceString.length()) {
			c = sourceString.charAt(index);
		}
		index++;
		return c;
	}

	public char lookUpChar(int i) {
		return (i < 0 || sourceString.length() <= i) ? '\0' : sourceString.charAt(i);
	}

	/* 
	 * returns the character to the document
	 */
	public void releaseChar() {
		if (index > 0) {
			index--;
		}
	}

	public boolean startsWith(String s) {
		int l = index + s.length();
		if(l > sourceString.length()) return false;
		for (int i = index; i < l; i++) {
			if(lookUpChar(i) != s.charAt(i - index)) {
				return false;
			}
		}
		return true;
	}

	public LexicalToken getLastToken() {
		return last;
	}

	public int getState() {
		return state;
	}

	public int getCurrentIndex() {
		return index;
	}

}
