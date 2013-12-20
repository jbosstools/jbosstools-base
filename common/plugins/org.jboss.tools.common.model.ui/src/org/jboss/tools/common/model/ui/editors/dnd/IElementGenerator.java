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
package org.jboss.tools.common.model.ui.editors.dnd;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.IScopeContext;
import org.eclipse.core.runtime.preferences.InstanceScope;
import org.eclipse.ui.texteditor.AbstractDecoratedTextEditorPreferenceConstants;

public interface IElementGenerator {
	public void setDataModel(Object object);
	public String generateStartTag(); 
	public String generateEndTag();	

	public static ElementNode SEPARATOR = new ElementNode(null, true) {
		@Override
		public void flush(NodeWriter writer, int indent) {
			writer.next();
		}
	};

	public static class NodeWriter {
		static final String ECLIPSE_EDITORS_PLUGIN = "org.eclipse.ui.editors";
		List<StringBuilder> builders = new ArrayList<StringBuilder>();
		StringBuilder current = null;
		boolean separate = true;
		String indentUnit = "\t";

	    public static int getTabWidth() {
			return Platform.getPreferencesService().getInt(ECLIPSE_EDITORS_PLUGIN, AbstractDecoratedTextEditorPreferenceConstants.EDITOR_TAB_WIDTH, 4, new IScopeContext[]{InstanceScope.INSTANCE});  //$NON-NLS-1$
		}

	    public static boolean useSpaces() {
			return Platform.getPreferencesService().getBoolean(ECLIPSE_EDITORS_PLUGIN, AbstractDecoratedTextEditorPreferenceConstants.EDITOR_SPACES_FOR_TABS, false, new IScopeContext[]{InstanceScope.INSTANCE});  //$NON-NLS-1$
	    }

	    public static String computeIndentUnit() {
	    	if(!useSpaces()) {
	    		return "\t";
	    	}
	    	int n = getTabWidth();
	    	StringBuilder sb = new StringBuilder(n);
	    	for (int i = 0; i < n; i++) sb.append(' ');
	    	return sb.toString();
	    }

		public NodeWriter(boolean separate) {
			next();
			this.separate = separate;
			indentUnit = computeIndentUnit();
		}
		
		public void next() {
			if(separate) {
				while(current != null && current.length() > 0 && (current.charAt(current.length() - 1) == '\r'
						|| current.charAt(current.length() - 1) == '\n')) {
					current.setLength(current.length() - 1);
				}
				current = new StringBuilder();
				builders.add(current);
			}
		}
		public NodeWriter append(String s) {
			current.append(s);
			return this;
		}

		public String[] getResult() {
			String[] result = new String[builders.size()];
			for (int i = 0; i < result.length; i++) result[i] = builders.get(i).toString(); 
			return result;
		}

		public String getText() {
			if(builders.size() == 1) {
				return builders.get(0).toString();
			}
			StringBuilder result = new StringBuilder();
			for (int i = 0; i < builders.size(); i++) result.append(builders.get(i).toString());
			return result.toString();
		}

		public void appendIndent(int indent) {
			for (int i = 0; i < indent; i++) {
				append(indentUnit);
			}
		}
	}

	public static class NamedNode {
		protected String name;
		
		public static String escapeHtml(String text, boolean isAttribute) {
			StringBuilder sb = new StringBuilder();
			for (int i = 0; i < text.length(); i++) {
				char ch = text.charAt(i);
				if(ch == '<') {
					sb.append("&lt;");
				} else if(ch == '>') {
					sb.append("&gt;");
				} else if(ch == '&' && !isEscapedSequence(text, i)) {
					sb.append("&amp;");
				} else if(isAttribute && ch == '"') {
					sb.append("&quot;");
				} else {
					sb.append(ch);
				}
			}
			return sb.toString();
		}

		static boolean isEscapedSequence(String text, int p) {
			if(text.charAt(p) != '&') {
				return false;
			}
			for (int i = p + 1; i < text.length(); i++) {
				char ch = text.charAt(i);
				if(ch == '&') return false;
				if(ch == ';') return true;
			}
			return false;
		}

		public void flush(NodeWriter sb, int indent) {
		}

	}

	public static class RootNode extends ElementNode {
		public static ElementNode newRoot() {
			return new RootNode();
		}
		
		public RootNode() {
			super(null, false);
		}

		@Override
		public void flush(NodeWriter sb, int indent) {
			for (NamedNode c: children) {
				c.flush(sb, indent);
			}
		}
	}

	public static class TextNode extends NamedNode {

		public TextNode(String text) {
			this.name = text;
		}

		@Override
		public void flush(NodeWriter sb, int indent) {
			if(indent >= 0) {
				sb.appendIndent(indent);
			}
			sb.append(name);
			if(indent >= 0) {
				sb.append("\n");
			}
		}
		
	}

	/**
	 * Stores in a simple way html content to be flushed into text.
	 *
	 */
	public static class ElementNode extends NamedNode {
		List<AttributeNode> attributes = new ArrayList<AttributeNode>();
		List<NamedNode> children = new ArrayList<NamedNode>();
		boolean empty;
		String text = null;

		public ElementNode(String name, boolean empty) {
			this.name = name;
			this.empty = empty;
		}

		public ElementNode(String name, String text) {
			this.name = name;
			this.text = (text == null) ? null : escapeHtml(text, false);
			this.empty = text == null;
		}

		public void addAttribute(String name, String value) {
			attributes.add(new AttributeNode(name, value));
		}

		public ElementNode addChild(String name) {
			ElementNode c = new ElementNode(name, true);
			children.add(c);
			empty = false;
			return c;
		}

		public void addTextChild(String text) {
			TextNode c = new TextNode(text);
			children.add(c);
			empty = false;
		}
		 
		public ElementNode addChild(String name, String text) {
			ElementNode c = new ElementNode(name, text);
			children.add(c);
			empty = false;
			return c;
		}
	
		public List<NamedNode> getChildren() {
			return children;
		}
		 
		public void flush(NodeWriter sb, int indent) {
			if(indent >= 0) {
				sb.appendIndent(indent);
			}
			sb.append("<").append(name);
			for (AttributeNode a: attributes) {
				a.flush(sb);
			}
			if(empty) {
				sb.append("/>");
			} else if(text != null) {
				sb.append(">");
				for (NamedNode c: children) {
					c.flush(sb, -1);
				}
				sb.append(text).append("</").append(name).append(">");
			} else {
				sb.append(">").append("\n");
				for (NamedNode c: children) {
					c.flush(sb, indent + 1);
				}
				sb.appendIndent(indent);
				sb.append("</").append(name).append(">");
			}
			if(indent >= 0) {
				sb.append("\n");
			}
		}
	}

	public static class AttributeNode extends NamedNode {
		String value;
		public AttributeNode(String name, String value) {
			this.name = name;
			this.value = escapeHtml(value, true);
		}

		public void flush(NodeWriter sb) {
			sb.append(" ").append(name).append("=\"").append(value).append("\"");
		}
	}

}
