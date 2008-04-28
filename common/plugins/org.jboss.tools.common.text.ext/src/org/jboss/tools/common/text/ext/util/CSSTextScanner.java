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
package org.jboss.tools.common.text.ext.util;

import java.io.InputStream;
import java.io.Reader;

import org.eclipse.jface.text.rules.ICharacterScanner;
import org.eclipse.jface.text.rules.IToken;

/**
 * @author Jeremy
 *
 */
public class CSSTextScanner extends TextScanner {
	public static final String CSS_CLASS_NAME = "___css_class_name";
	public static final String CSS_CLASS_NAME_SEPARATOR = "___css_class_name_separator";
	public static final String CSS_CLASS_BODY = "___css_class_body";
	public static final String CSS_CLASS_COMMENT = "___css_class_comment";
	
	private static final int STATE_START			= 0;
	private static final int STATE_NAME				= 1;
	private static final int STATE_NAME_SEPARATOR	= 2;
	private static final int STATE_BODY				= 3;
	private static final int STATE_COMMENT			= 4;
	private static final int STATE_END				= 5;

	private int state;
	private int savedForCommentState;
	
	/**
	 * @param stream
	 */
	public CSSTextScanner(InputStream stream) {
		super(stream);
		this.state = STATE_START;
	}
	/**
	 * @param reader
	 */
	public CSSTextScanner(Reader reader) {
		super(reader);
		this.state = STATE_START;
	}

	private void setState(int newState) {
		if (this.state != STATE_COMMENT)
			this.savedForCommentState = this.state;
		this.state = newState;
	}
	
	/* (non-Javadoc)
	 * @see org.jboss.tools.jsf.text.ext.util.TextScanner#nextToken()
	 */
	public IToken nextToken() {
		offset += length;
		switch (state) {
			case STATE_START:
			case STATE_NAME_SEPARATOR:
				return nextNameSeparatorToken();
			case STATE_NAME:
				return nextNameToken();
			case STATE_BODY:
				return nextBodyToken();
			case STATE_COMMENT:
				return nextCommentToken();
		}
		return nextEndToken();
	}
	
	private IToken nextNameToken() {
		// Check first one char in the stream
		int ch = read();
		if (ch == ICharacterScanner.EOF) {
			return getToken(CSS_CLASS_NAME);
		}
		if (!NMTOKEN_DETECTOR.isWordStart((char)ch)) {
			// Emulate END of text
			clearText();
			return getToken(CSS_CLASS_NAME);
		}

		ch = read();
		int readsCount = 0;
		while (ch != ICharacterScanner.EOF) {
			if (!NMTOKEN_DETECTOR.isWordPart((char)ch)) {
				unread();
				state = STATE_NAME_SEPARATOR;
				return getToken(CSS_CLASS_NAME);
			}
			readsCount++;
			ch = read();
		}
		return getToken(CSS_CLASS_NAME);
	}

	private IToken nextNameSeparatorToken() {
		int count = skipWhitespaceToken();
		if (count > 0) {
			state = STATE_NAME_SEPARATOR;
			return getToken(null);
		}
		int ch = read();
		int readsCount = 0;
		while (ch != ICharacterScanner.EOF) {
			if (NMTOKEN_DETECTOR.isWordStart((char)ch)) {
				unread();
				state = STATE_NAME;
				return (readsCount > 0 ? getToken(CSS_CLASS_NAME_SEPARATOR) : nextNameToken());
			}
			if (WS_DETECTOR.isWhitespace((char)ch)) {
				unread();
				state = STATE_NAME_SEPARATOR;
				return (readsCount > 0 ? getToken(CSS_CLASS_NAME_SEPARATOR) : nextNameSeparatorToken());
			}
			if (ch == '{') {
				unread();
				state = STATE_BODY;
				return (readsCount > 0 ? getToken(CSS_CLASS_NAME_SEPARATOR) : nextBodyToken());
			}
			if (ch == '/') {
				ch = read();
				if (ch == ICharacterScanner.EOF) break; 
				if (ch == '*') {
					// Comment openning
					unread(); // Unread '*'-char
					unread(); // Unread '/'-char
					setState(STATE_COMMENT);
					return (readsCount > 0 ? getToken(CSS_CLASS_NAME_SEPARATOR) : nextCommentToken());
				}
				// Broken file ??? Emulate END of text
				clearText();
				return getToken(CSS_CLASS_NAME_SEPARATOR);
			}
			readsCount++;
			ch = read();
		}
		state = STATE_END;
		return getToken(CSS_CLASS_NAME_SEPARATOR);
	}

	private IToken nextBodyToken() {
		int level = 0;
		int ch = read();
		while (ch != ICharacterScanner.EOF) {
			if (ch == '{') {
				level++;
			} else if (ch == '}') {
				level--;
				if (level <= 0) {
					state = STATE_START;
					return getToken(CSS_CLASS_BODY);
				}
			}
			if (ch == '/') {
				ch = read();
				if (ch == ICharacterScanner.EOF) break; 
				if (ch == '*') {
					// Comment openning
					unread(); // Unread '*'-char
					unread(); // Unread '/'-char
					setState(STATE_COMMENT);
					return getToken(CSS_CLASS_BODY);
				}
				// Broken file ??? Emulate END of text
				clearText();
				return getToken(CSS_CLASS_BODY);
			}
			ch = read();
		}
		state = STATE_END;
		return getToken(CSS_CLASS_BODY);
	}
	
	private IToken nextCommentToken() {
		int ch = read();
		while (ch != ICharacterScanner.EOF) {
			if (ch == '*') {
				ch = read();
				if (ch == ICharacterScanner.EOF) break; 
				if (ch == '/') {
					// Comment closing
					setState(savedForCommentState);
					return getToken(CSS_CLASS_COMMENT);
				}
				unread(); // Unread last char 
			}
			ch = read();
		}
		setState(savedForCommentState);
		return getToken(CSS_CLASS_COMMENT);
	}
	
	private IToken nextEndToken () {
		state = STATE_END;
		// Emulate END of text
		clearText();
		return getToken(null);
	}

}
