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
package org.jboss.tools.common.model.util;

public class ELParser {
	public static int NONE = 0;
	public static int OPEN = 1;
	public static int CLOSE = 2;
	public static int NAME = 3;
	public static int DOT = 4;
	public static int OPEN_ARG = 5;
	public static int CLOSE_ARG = 6;	
	public static int ARGUMENT = 7;
	public static int SPACES = 8;
	
	
	static String[] TOKENS = {"+", "{", "}", "NAME", ".", "['", "']", "ARG", "_"};

	public class Token {
		public int start;
		public int end;
		public int kind;
		public Token previous;
		public Token next;
		
		public String toString() {
			return "[" + TOKENS[kind] + "]" + "(" + start + ":" + end + ")";
		}
		
		Token add(int kind) {
			Token t = new Token();
			t.start = end;
			t.kind = kind;
			next = t;
			next.previous = this;
			return t;
		}
	}
	
	Token root;
	
	public Token parse(String s) {
		root = new Token();
		root.kind = NONE;
		root.start = 0;
		Token current = root;
		int state = 0; 
		for (int i = 0; i < s.length(); i++) {
			char c = s.charAt(i);
			if(isExpressionStart(s, i)) {
				current = startNewToken(current, i, OPEN);
				current = startNewToken(current, i + 2, NONE);
				state = 1;
				i++;
				continue;
			} else if(isIncompleteExpressionStart(s, i)) {
				current = startNewToken(current, i, OPEN);
				current = startNewToken(current, i + 1, NONE);
				state = 1;
			} else if(c == '}') {
				current = startNewToken(current, i, CLOSE);
				current = startNewToken(current, i + 1, NONE);
				state = 1;
				continue;
			}
			switch (state) {
				case 0:
//					break;
				case 1:
					if(Character.isWhitespace(c)) {
						current = startNewToken(current, i, SPACES);
						state = 16;
					} else if(Character.isJavaIdentifierStart(c)) {
						current.kind = NAME;
						state = 2;
					} else {
						state = 3;
					}
					break;
				case 2:
					if(c == '[') {
						current = startNewToken(current, i, OPEN_ARG);
						if(i < s.length() - 1 && s.charAt(i + 1) == '\'') {
							current = startNewToken(current, i + 2, ARGUMENT);
							++i;
						} else {
							current = startNewToken(current, i + 1, ARGUMENT);
						}
						state = 5;
					} else if(c == '.') {
						current = startNewToken(current, i, DOT);
						current = startNewToken(current, i + 1, NONE);
						state = 1;
					} else if(Character.isWhitespace(c)) {
						current = startNewToken(current, i, SPACES);
						state = 26;
					} else if(Character.isJavaIdentifierPart(c)) {
						//continue
					} else {
						current = startNewToken(current, i, NONE);
						state = 3;
					}
					break;
				case 3:
					if(Character.isJavaIdentifierStart(c)) {
						current = startNewToken(current, i, NAME);
						state = 2;
					} else if(Character.isWhitespace(c)) {
						current = startNewToken(current, i, SPACES);
						state = 26;
					}
					break;
				case 4:
					if(c == ']') {
						current = startNewToken(current, i + 1, NONE);
						state = 1;
					} else if(Character.isJavaIdentifierPart(c) || c == '.') {
						//continue
					} else if(Character.isWhitespace(c)) {
						current = startNewToken(current, i, SPACES);
						state = 46;
					} else {
						current = startNewToken(current, i, NONE);
						state = 3;
					}
					break;
				case 5:
					if(c == ']') {
						if(c > 0 && s.charAt(i - 1) == '\'') {
							current.end = i - 1;
						} else {
							current.end = i;
						}
						current = current.add(CLOSE_ARG);
						current = startNewToken(current, i + 1, NONE);
						state = 1;
					}
					break;
				//reading whitespace
				case 16:
					if(Character.isWhitespace(c)) {
						//continue
					} else if(Character.isJavaIdentifierStart(c)) {
						current = startNewToken(current, i, NAME);
						state = 2;
					} else if(current.kind == SPACES) {
						current = startNewToken(current, i, NONE);
						state = 3;
					} else {
						state = 3;
					}
					break;
				case 26:
					if(c == '[') {
						current = startNewToken(current, i, OPEN_ARG);
						if(i < s.length() - 1 && s.charAt(i + 1) == '\'') {
							current = startNewToken(current, i + 2, ARGUMENT);
							++i;
						} else {
							current = startNewToken(current, i + 1, ARGUMENT);
						}
						state = 5;
					} else if(c == '.') {
						current = startNewToken(current, i, DOT);
						current = startNewToken(current, i + 1, NONE);
						state = 1;
					} else if(Character.isWhitespace(c)) {
						//continue
					} else if(current.kind == SPACES && Character.isJavaIdentifierStart(c)) {
						if(current.previous != null && current.previous.kind == NAME) {
							current.kind = NONE;
							current = startNewToken(current, i, NAME);
							state = 2;
						} else {
							current = startNewToken(current, i, NAME);
							state = 2;
						}
					} else {
						current = startNewToken(current, i, NONE);
						state = 3;
					}
					break;
				case 46:
					if(c == ']') {
						current = startNewToken(current, i + 1, NONE);
						state = 1;
					} else if(Character.isJavaIdentifierPart(c) || c == '.') {
						current = startNewToken(current, i, NAME);
						state = 2;
					} else if(Character.isWhitespace(c)) {
						//continue
					} else {
						current = startNewToken(current, i, NONE);
						state = 3;
					}
					break;
				default:
					break;
			}
		}
		current.end = s.length();
		if(current.start == current.end && current.previous != null) {
			current = current.previous;
			current.next = null;
		}
//		Token c = root;
//		ModelPlugin.log(s);
//		while(c != null) {
//			ModelPlugin.log(c.toString());
//			c = c.next;
//		}
		return root;
	}
	
	private Token startNewToken(Token current, int i, int kind) {
		if(current.start < i) {
			current.end = i;
			current = current.add(kind);
		} else {
			current.kind = kind;
		}
		return current;
	}
	
	private boolean isExpressionStart(String s, int offset) {
		if(offset >= s.length() - 1) return false;
		char c1 = s.charAt(offset);
		char c2 = s.charAt(offset + 1);
		return (c1 == '$' || c1 == '#') && c2 == '{';
	}
	private boolean isIncompleteExpressionStart(String s, int offset) {
		if(offset >= s.length()) return false;
		char c1 = s.charAt(offset);
		return (c1 == '$' || c1 == '#');
	}
	
	public static boolean isPartOfCall(int kind) {
		return kind == ARGUMENT || kind == CLOSE_ARG || kind == OPEN_ARG || kind == DOT || kind == NAME || kind == SPACES;
	}
	
	public static Token getTokenAt(Token root, int offset) {
		Token c = root;
		while(c.end < offset && c.next != null) c = c.next;
		return c;
	}
	public static Token getCallStart(Token t) {
		if(t == null || !isPartOfCall(t.kind)) return null;
		Token c = t;
		while(c != null && c.previous != null && isPartOfCall(c.previous.kind)) c = c.previous;
		if(c != null && c.kind == SPACES) {
			if(c.next == null) return null;
			c = c.next;
		}
		if(c == null || !isPartOfCall(c.kind)) return null;
		return c;
	}
	public static Token getCallEnd(Token t) {
		if(t == null || !isPartOfCall(t.kind)) return null;
		Token c = t;
		while(c != null && c.next != null && isPartOfCall(c.next.kind)) c = c.next;
		if(c != null && c.kind == SPACES && c.previous != null) {
			if(c.previous == null) return null;
			c = c.previous;
		}
		if(c == null || c.kind == SPACES) return null;
		return c;
	}
	public static Token getPrecedingOpen(Token t, int offset) {
		if(t == null || t.kind == CLOSE) return null;
		if(t.kind == OPEN && t.start < offset) return t;
		Token c = t.previous;
		while(true) {
			if(c == null || c.kind == CLOSE) return null;
			if(c.kind == OPEN) return c;
			c = c.previous;
		}
	}
	public static boolean isFollowedByClose(Token t, int offset) {
		if(t == null) return false;
		if(t.kind == CLOSE) return offset < t.end;
		if(t.kind == OPEN && t.start == offset) return false;
		Token c = t.next;
		while(true) {
			if(c == null || c.kind == OPEN) return false;
			if(c.kind == CLOSE) return true;
			c = c.next;
		}
	}
	
	public static Token findLastCall(Token token) {
		if(token == null) return null;
		Token last = token;
		while(last.next != null) last = last.next;
		if(!isPartOfCall(last.kind)) return null;
		while(last.previous != null && isPartOfCall(last.previous.kind)) last = last.previous;
		return last;
	}
	
}
