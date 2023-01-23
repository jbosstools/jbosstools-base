/******************************************************************************
 * Copyright (c) 2005 BEA Systems, Inc.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *    Konstantin Komissarchik - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.common.core.classpath;

import java.io.StringReader;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.NoSuchElementException;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.InstanceScope;
import org.eclipse.jdt.core.IClasspathAttribute;
import org.jboss.tools.common.core.CommonCorePlugin;
import org.jboss.tools.common.core.Messages;
import org.osgi.service.prefs.BackingStoreException;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;

/**
 * @author <a href="mailto:kosta@bea.com">Konstantin Komissarchik</a>
 */

public final class ClasspathDecorationsManager {
	private static final String CLASSPATH_PREFERENCES = "classpathPreferences"; //$NON-NLS-1$
	private static final String SEPARATOR = System
			.getProperty("line.separator"); //$NON-NLS-1$
	private final HashMap decorations;

	public ClasspathDecorationsManager() {
		this.decorations = read();
	}

	private IEclipsePreferences getEclipsePreferences() {
		IEclipsePreferences node = (IEclipsePreferences) Platform
				.getPreferencesService().getRootNode()
				.node(InstanceScope.SCOPE).node(CommonCorePlugin.PLUGIN_ID);
		return node;
	}

	private String getPreferences() {
		return getEclipsePreferences().get(CLASSPATH_PREFERENCES, null);
	}

	public ClasspathDecorations getDecorations(final String key,
			final String entry) {
		final HashMap submap = (HashMap) this.decorations.get(key);

		if (submap == null) {
			return null;
		}

		return (ClasspathDecorations) submap.get(entry);
	}

	public void setDecorations(final String key, final String entry,
			final ClasspathDecorations dec) {
		HashMap submap = (HashMap) this.decorations.get(key);

		if (submap == null) {
			submap = new HashMap();
			this.decorations.put(key, submap);
		}

		submap.put(entry, dec);
	}

	public void clearAllDecorations(final String key) {
		this.decorations.remove(key);
	}

	public void save() {
		StringBuffer buffer = new StringBuffer();
		buffer.append("<classpath>"); //$NON-NLS-1$
		buffer.append(SEPARATOR);
		for (Iterator itr1 = decorations.entrySet().iterator(); itr1.hasNext();) {
			final Map.Entry entry1 = (Map.Entry) itr1.next();
			final Map submap = (Map) entry1.getValue();

			buffer.append("  <container id=\""); //$NON-NLS-1$
			buffer.append((String) entry1.getKey());
			buffer.append("\">"); //$NON-NLS-1$
			buffer.append(SEPARATOR);

			for (Iterator itr2 = submap.entrySet().iterator(); itr2.hasNext();) {
				final Map.Entry entry2 = (Map.Entry) itr2.next();

				final ClasspathDecorations dec = (ClasspathDecorations) entry2
						.getValue();

				buffer.append("    <entry id=\""); //$NON-NLS-1$
				buffer.append((String) entry2.getKey());
				buffer.append("\">"); //$NON-NLS-1$
				buffer.append(SEPARATOR);

				String src = ""; //$NON-NLS-1$
				if (dec.getSourceAttachmentPath() != null) {
					src = dec.getSourceAttachmentPath().toString();
				}
				buffer.append("      <source-attachment-path>"); //$NON-NLS-1$
				buffer.append(src);
				buffer.append("</source-attachment-path>"); //$NON-NLS-1$
				buffer.append(SEPARATOR);

				if (dec.getSourceAttachmentRootPath() != null) {
					buffer.append("      <source-attachment-root-path>"); //$NON-NLS-1$
					buffer.append(dec.getSourceAttachmentRootPath().toString());
					buffer.append("</source-attachment-root-path>"); //$NON-NLS-1$
					buffer.append(SEPARATOR);
				}

				final IClasspathAttribute[] attrs = dec.getExtraAttributes();

				for (int i = 0; i < attrs.length; i++) {
					final IClasspathAttribute attr = attrs[i];

					buffer.append("      <attribute name=\""); //$NON-NLS-1$
					buffer.append(attr.getName());
					buffer.append("\">"); //$NON-NLS-1$
					buffer.append(attr.getValue());
					buffer.append("</attribute>"); //$NON-NLS-1$
					buffer.append(SEPARATOR);
				}

				buffer.append("    </entry>"); //$NON-NLS-1$
				buffer.append(SEPARATOR);
			}

			buffer.append("  </container>"); //$NON-NLS-1$
			buffer.append(SEPARATOR);
		}

		buffer.append("</classpath>"); //$NON-NLS-1$
		buffer.append(SEPARATOR);
		IEclipsePreferences ep = getEclipsePreferences();
		ep.put(CLASSPATH_PREFERENCES, buffer.toString());
		try {
			ep.flush();
		} catch (BackingStoreException e) {
			String msg = Messages.ClasspathDecorationsManager_unexpected_exception;
			CommonCorePlugin.getPluginLog().logError(msg, e);
		}
	}

	private HashMap read() {
		final HashMap map = new HashMap();
		String prefs = getPreferences();
		if (prefs == null || prefs.length() <= 0)
			return map;

		Element root = null;
		try {
			final DocumentBuilderFactory factory = DocumentBuilderFactory
					.newInstance();

			final DocumentBuilder docbuilder = factory.newDocumentBuilder();

			StringReader reader = new StringReader(prefs);
			InputSource source = new InputSource(reader);
			root = docbuilder.parse(source).getDocumentElement();
		} catch (Exception e) {
			String msg = Messages.ClasspathDecorationsManager_unexpected_exception;
			CommonCorePlugin.getPluginLog().logError(msg, e);
			return map;
		}

		for (Iterator itr1 = elements(root, "container"); itr1.hasNext();) //$NON-NLS-1$
		{
			final Element e1 = (Element) itr1.next();
			final String cid = e1.getAttribute("id"); //$NON-NLS-1$

			final HashMap submap = new HashMap();
			map.put(cid, submap);

			for (Iterator itr2 = elements(e1, "entry"); itr2.hasNext();) //$NON-NLS-1$
			{
				final Element e2 = (Element) itr2.next();
				final String eid = e2.getAttribute("id"); //$NON-NLS-1$
				final ClasspathDecorations dec = new ClasspathDecorations();

				submap.put(eid, dec);

				for (Iterator itr3 = elements(e2); itr3.hasNext();) {
					final Element e3 = (Element) itr3.next();
					final String n = e3.getNodeName();
					String text = text(e3);
					if (text != null) {
						if (n.equals("source-attachment-path")) //$NON-NLS-1$
						{
							dec.setSourceAttachmentPath(new Path(text(e3)));
						} else if (n.equals("source-attachment-root-path")) //$NON-NLS-1$
						{
							dec.setSourceAttachmentRootPath(new Path(text(e3)));
						}
					}
					if (n.equals("attribute")) //$NON-NLS-1$
					{
						final String name = e3.getAttribute("name"); //$NON-NLS-1$
						dec.addExtraAttribute(name, text(e3));
					}

				}
			}
		}

		return map;
	}

	private static String text(final Element el) {
		final NodeList nodes = el.getChildNodes();

		String str = null;
		StringBuffer buf = null;

		for (int i = 0, n = nodes.getLength(); i < n; i++) {
			final Node node = nodes.item(i);

			if (node.getNodeType() == Node.TEXT_NODE) {
				final String val = node.getNodeValue();

				if (buf != null) {
					buf.append(val);
				} else if (str != null) {
					buf = new StringBuffer();
					buf.append(str);
					buf.append(val);

					str = null;
				} else {
					str = val;
				}
			}
		}

		if (buf != null) {
			return buf.toString();
		}
		return str;
	}

	private static Iterator elements(final Element el, final String name) {
		return new ElementsIterator(el, name);
	}

	private static Iterator elements(final Element el) {
		return new ElementsIterator(el, null);
	}

	private static final class ElementsIterator implements Iterator {
		private final NodeList nodes;
		private final int length;
		private final String name;
		private int position;
		private Element element;

		public ElementsIterator(final Element parent, final String name) {
			this.nodes = parent.getChildNodes();
			this.length = nodes.getLength();
			this.position = -1;
			this.name = name;

			advance();
		}

		private void advance() {
			this.element = null;
			this.position++;

			for (; this.position < this.length && this.element == null; this.position++) {
				final Node node = this.nodes.item(this.position);

				if (node.getNodeType() == Node.ELEMENT_NODE
						&& (this.name == null || node.getNodeName().equals(
								this.name))) {
					this.element = (Element) node;
				}
			}
		}

		public boolean hasNext() {
			return (this.element != null);
		}

		public Object next() {
			final Element el = this.element;

			if (el == null) {
				throw new NoSuchElementException();
			}

			advance();

			return el;
		}

		public void remove() {
			throw new UnsupportedOperationException();
		}
	}

}
