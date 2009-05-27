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
package org.jboss.tools.common.kb;

import java.util.ArrayList;
import java.util.Collection;
import java.util.StringTokenizer;

/**
 * Describes query to store.
 * @see KbStore
 * @author eskimo
 */
public class KbQuery {

	public static final String XML_DECLARATION_QUERY = "?"; //$NON-NLS-1$
	public static final String TAG_SEPARATOR = "/"; //$NON-NLS-1$
	public static final String ATTRIBUTE_SEPARATOR = "@"; //$NON-NLS-1$
	public static final String PREFIX_SEPARATOR = ":"; //$NON-NLS-1$
	public static final String ENUMERATION_SEPARATOR = "="; //$NON-NLS-1$
	public static final String DONT_FILTER_END_TAG_CHAR = "^"; //$NON-NLS-1$
	public static final String JSP_DIRECTIVE_QUERY = "@"; //$NON-NLS-1$
	private static final char START_CODE_CHAR = '%';
	private static final char END_CODE_CHAR = ';';

	private String query = ""; //$NON-NLS-1$
	private Collection resources = new ArrayList();
	private Collection dinamicResources = new ArrayList();

	private String lastTagName;
	private String lastTag;
	private String notFullLastTag;
	private String lastPrefixName;

	/**
	 * Constructor
	 * @param query - String query. For example - /h:otputText@value=#{
	 */
	public KbQuery(String query) {
		this.query = query;
	}

	/**
	 * Constructor
	 * @param query - String query. For example - /h:otputText@value=#{
	 * @param resources - Query only this resorces from store. 
	 */
	public KbQuery(String query, Collection resources) {
		this.query = query;
		this.resources = resources;
	}

	/**
	 * Constructor
	 * @param query - String query. For example - /h:otputText@value=#{
	 * @param resources - Query only this resorces from store.
	 * @param dinamicResources - Query only this dinamic resorces from store.
	 */
	public KbQuery(String query, Collection resources, Collection dinamicResources) {
		this.query = query;
		this.resources = resources;
		this.dinamicResources = dinamicResources;
	}

	/**
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		StringBuffer result = new StringBuffer();
		result.append("Query string = ["); //$NON-NLS-1$
		result.append(query);
		result.append("]; Resources size = ["); //$NON-NLS-1$
		result.append(resources.size());
		result.append("];"); //$NON-NLS-1$
		return result.toString();
	}

	/**
	 * @return String query
	 */
	public String getQuery() {
		return query;
	}

	/**
	 * @return
	 */
	public Collection getResources() {
		return resources;
	}

	/**
	 * @return
	 */
	public Collection getDinamicResources() {
		return dinamicResources;
	}

	/**
	 * @param list
	 */
	public void setResources(Collection list) {
		resources = list;
	}

	/**
	 * Encode string query
	 * @param str
	 * @return
	 */
	public static String encode(String str) {
		if((str.indexOf('/')<0)&&(str.indexOf(START_CODE_CHAR)<0)) {
			return str;
		}
		char[] chars = str.toCharArray();
		StringBuffer result = new StringBuffer(str.length());
		for(int i = 0; i < chars.length; i++) {
			if((chars[i]!='/')&&(chars[i]!=START_CODE_CHAR)) {
				result.append(chars[i]);
			} else {
				result.append(START_CODE_CHAR);
				result.append((int)chars[i]);
				result.append(END_CODE_CHAR);
			}
		}
		return result.toString();
	}

	/**
	 * Decode string query
	 * @param str
	 * @return
	 */
	public static String decode(String str) {
		if(str.indexOf(START_CODE_CHAR)<0) {
			return str;
		}
		StringBuffer result = new StringBuffer(str.length());
		StringTokenizer st = new StringTokenizer(str, "" + START_CODE_CHAR, false); //$NON-NLS-1$
		if((!str.startsWith("" + START_CODE_CHAR))&&(st.hasMoreElements())) { //$NON-NLS-1$
			result.append(st.nextElement());
		}
		while(st.hasMoreElements()) {
			String s = st.nextToken();
			int endCode = s.indexOf(END_CODE_CHAR);
			if(endCode!=-1) {
				try {
					int intChar = Integer.parseInt(s.substring(0, endCode));
					char chr = (char)intChar;
					result.append(chr);
					if(++endCode<s.length()) {
						result.append(s.substring(endCode));
					}
				} catch(NumberFormatException e) {
					String message = "ERROR: can't decode string=" + s; //$NON-NLS-1$
					KbPlugin.getPluginLog().logError(message, e);
				}
			}
		}
		return result.toString();
	}

	/**
	 * 
	 * @return prefix name of last tag from string query.
	 */
	public String getLastTagPrefixName() {
		if(lastPrefixName==null) {
			String lastTag = getNotFullLastTag();
			if(lastTag==null) {
				return null;
			}
			int startTagName = lastTag.indexOf(KbQuery.PREFIX_SEPARATOR);
			if(startTagName < 0){
				return null;
			}
			lastPrefixName = lastTag.substring(0, startTagName);
		}
		return lastPrefixName;
	}

	/**
	 * 
	 * @return name of last tag from string query.
	 */
	public String getLastTagName() {
		if(lastTagName!=null) {
			return lastTagName;
		}
		String lastTag = getNotFullLastTag();
		if(lastTag==null) {
			return null;
		}
		int startTagName = lastTag.indexOf(KbQuery.PREFIX_SEPARATOR);
		if(startTagName < 0){
			startTagName = 0;
		} else {
			startTagName+=KbQuery.PREFIX_SEPARATOR.length();
		}
		int endTagName = lastTag.indexOf(KbQuery.ATTRIBUTE_SEPARATOR);
		if(endTagName<0) {
			endTagName = lastTag.length();
		}
		if(startTagName>=endTagName) {
			return null;
		}
		lastTagName = lastTag.substring(startTagName, endTagName);
		return lastTagName;
	}

	/**
	 * 
	 * @return last tag from string query.
	 */
	public String getLastTag() {
		if(lastTag==null) {
			lastTag = getLastTag(false);
		}
		return lastTag;
	}

	private String getNotFullLastTag() {
		if(notFullLastTag==null) {
			notFullLastTag = getLastTag(true);
		}
		return notFullLastTag;
	}

	/**
	 * 
	 * @return name of last tag whish is complete.
	 * For example for quert "/h:form/h:outputT" it will be "h:form".
	 */
	public String getFullLastTagName() {
		String prefix = getLastTagPrefixName();
		String name = getLastTagName();
		StringBuffer result = new StringBuffer();
		if(prefix!=null) {
			result.append(prefix).append(PREFIX_SEPARATOR);
		}
		if(name!=null) {
			result.append(name);
		}
		return result.length()>0?result.toString():null;
	}

	/**
	 * 
	 * @param notFullTagName
	 * @return
	 */
	public String getLastTag(boolean notFullTagName) {
		StringTokenizer tags = new StringTokenizer(query, KbQuery.TAG_SEPARATOR, true);
		String lastTag = null;

		while(tags.hasMoreTokens()) {
			String tag = tags.nextToken();
			if(tag.equals(KbQuery.TAG_SEPARATOR)) {
				continue;
			}
			if(tags.hasMoreTokens() || notFullTagName) {
				lastTag = tag;
			}
		}

		return lastTag;
	}

	/**
	 * Strip every tag from query but last tag.
	 * @return
	 */
	public String getFullQueryForLastTag() {
		String lastTag = getLastTag();
		if(lastTag == null) {
			return null;
		}
		int startLastTag = query.lastIndexOf(lastTag);

		return query.substring(0, startLastTag);
	}
}