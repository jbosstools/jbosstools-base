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
		List<StringBuilder> builders = new ArrayList<StringBuilder>();
		StringBuilder current = null;
		boolean separate = true;

		public NodeWriter(boolean separate) {
			next();
			this.separate = separate;
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
			for (ElementNode c: children) {
				c.flush(sb, indent);
			}
		}
	}

	/**
	 * Stores in a simple way html content to be flushed into text.
	 *
	 */
	public static class ElementNode extends NamedNode {
		List<AttributeNode> attributes = new ArrayList<AttributeNode>();
		List<ElementNode> children = new ArrayList<ElementNode>();
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
		 
		public ElementNode addChild(String name, String text) {
			ElementNode c = new ElementNode(name, text);
			children.add(c);
			empty = false;
			return c;
		}
	
		public List<ElementNode> getChildren() {
			return children;
		}
		 
		public void flush(NodeWriter sb, int indent) {
			if(indent >= 0) {
				addIndent(sb, indent);
			}
			sb.append("<").append(name);
			for (AttributeNode a: attributes) {
				a.flush(sb);
			}
			if(empty) {
				sb.append("/>");
			} else if(text != null) {
				sb.append(">");
				for (ElementNode c: children) {
					c.flush(sb, -1);
				}
				sb.append(text).append("</").append(name).append(">");
			} else {
				sb.append(">").append("\n");
				for (ElementNode c: children) {
					c.flush(sb, indent + 1);
				}
				addIndent(sb, indent);
				sb.append("</").append(name).append(">");
			}
			if(indent >= 0) {
				sb.append("\n");
			}
		}
		private void addIndent(NodeWriter sb, int indent) {
			for (int i = 0; i < indent; i++) {
				sb.append("  ");
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
