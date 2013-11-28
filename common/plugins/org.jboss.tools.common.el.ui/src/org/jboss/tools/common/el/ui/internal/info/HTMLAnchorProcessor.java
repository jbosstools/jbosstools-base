/*******************************************************************************
 * Copyright (c) 2013 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.common.el.ui.internal.info;

import java.io.IOException;
import java.io.PushbackReader;
import java.io.Reader;
import java.util.HashSet;
import java.util.Set;

import org.eclipse.jface.internal.text.html.SubstitutionTextReader;

/**
 * Excludes anchors with unknown protocol schema in 'href' attributes
 * 
 * @author Victor V. Rubezhny
 *
 */
public class HTMLAnchorProcessor extends SubstitutionTextReader {
	private static final String EMPTY_STRING= ""; //$NON-NLS-1$

	Set<String> fEnabledProtocols = new HashSet<String>(2);
	private boolean fIgnoredAnchor= false;
	
	public HTMLAnchorProcessor(Reader reader, String[] enabledProtocols) {
		super(new PushbackReader(reader));
		if (enabledProtocols != null) {
			for (String p : enabledProtocols) {
				if (p != null && p.trim().length() > 0)
					fEnabledProtocols.add(p.trim().toLowerCase());
			}
		}
	}

	@Override
	protected String computeSubstitution(int c) throws IOException {
		if (c == '<')
			return  processHTMLTag();

		return null;
	}

	/*
	 * A '<' has been read. Process a html tag
	 */
	private String processHTMLTag() throws IOException {

		StringBuffer buf= new StringBuffer();
		int ch;
		do {

			ch= nextChar();

			while (ch != -1 && ch != '>') {
				buf.append((char) ch);
				ch= nextChar();
				if (ch == '"'){
					buf.append((char) ch);
					ch= nextChar();
					while (ch != -1 && ch != '"'){
						buf.append((char) ch);
						ch= nextChar();
					}
				}
				if (ch == '<' && !isInComment(buf)) {
					unread(ch);
					return '<' + buf.toString();
				}
			}

			if (ch == -1)
				return null;

			if (!isInComment(buf) || isCommentEnd(buf)) {
				break;
			}
			// unfinished comment
			buf.append((char) ch);
		} while (true);

		return internalProcessTag(buf.toString());
	}
	
	private void unread(int ch) throws IOException {
		((PushbackReader) getReader()).unread(ch);
	}
	
	private static boolean isInComment(StringBuffer buf) {
		return buf.length() >= 3 && "!--".equals(buf.substring(0, 3)); //$NON-NLS-1$
	}

	private static boolean isCommentEnd(StringBuffer buf) {
		int tagLen= buf.length();
		return tagLen >= 5 && "--".equals(buf.substring(tagLen - 2)); //$NON-NLS-1$
	}
	
	private String internalProcessTag(String tagText) {

		if (tagText == null || tagText.length() == 0)
			return EMPTY_STRING;

		String html= tagText.toLowerCase();

		String tag= html;
		if ('/' == tag.charAt(0))
			tag= tag.substring(1);

		if ("a".equals(html) || (html.length() > 2 && html.startsWith("a") && Character.isWhitespace(html.charAt(1)))) { //$NON-NLS-1$ $NON-NLS-2$
			return startAnchor(html) ? EMPTY_STRING : '<' + tagText + '>';
		}

		if ("/a".equals(html)) { //$NON-NLS-1$
			return stopAnchor() ? EMPTY_STRING : '<' + tagText + '>';
		}
		
		return '<' + tagText + '>';
	}
	
	private boolean startAnchor(String html) {
		fIgnoredAnchor = true;
		int hrefStart = html.indexOf("href"); //$NON-NLS-1$
		if (hrefStart != -1) {
			int equalStart = html.indexOf('=', hrefStart);
			if (equalStart != -1) {
				if ("href".equals(html.substring(hrefStart, equalStart).trim())) {
					int valueStart = equalStart + 1;
					while (valueStart < html.length() && !Character.isLetterOrDigit(html.charAt(valueStart)) &&
							('"' == html.charAt(valueStart) || '\'' == html.charAt(valueStart) || 
								Character.isWhitespace(html.charAt(valueStart)))) {
						valueStart++;
					}
					int end = html.indexOf(':', valueStart);
					if (end != -1) {
						String prot = html.substring(valueStart, end);
						fIgnoredAnchor = !fEnabledProtocols.contains(prot);
					}
				}
			}
		}
		return fIgnoredAnchor;
	}
	
	private boolean stopAnchor() {
		boolean result = fIgnoredAnchor;
		if (fIgnoredAnchor) {
			fIgnoredAnchor = false;
		}
		return result;
	}
}
