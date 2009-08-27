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
package org.jboss.tools.common.text.ext.hyperlink;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.Region;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.ide.IDE;
import org.eclipse.wst.css.core.internal.provisional.adapters.IModelProvideAdapter;
import org.eclipse.wst.css.core.internal.provisional.adapters.IStyleSheetAdapter;
import org.eclipse.wst.css.core.internal.provisional.document.ICSSDocument;
import org.eclipse.wst.css.core.internal.provisional.document.ICSSModel;
import org.eclipse.wst.css.core.internal.provisional.document.ICSSNode;
import org.eclipse.wst.css.core.internal.provisional.document.ICSSStyleRule;
import org.eclipse.wst.css.core.internal.provisional.document.ICSSStyleSheet;
import org.eclipse.wst.html.core.internal.htmlcss.LinkElementAdapter;
import org.eclipse.wst.html.core.internal.htmlcss.URLModelProvider;
import org.eclipse.wst.sse.core.internal.provisional.INodeNotifier;
import org.eclipse.wst.sse.core.internal.provisional.IStructuredModel;
import org.eclipse.wst.sse.core.internal.provisional.IndexedRegion;
import org.eclipse.wst.xml.core.internal.provisional.document.IDOMDocument;
import org.eclipse.wst.xml.core.internal.provisional.document.IDOMElement;
import org.eclipse.wst.xml.core.internal.provisional.document.IDOMModel;
import org.eclipse.wst.xml.core.internal.provisional.document.IDOMNode;
import org.jboss.tools.common.text.ext.ExtensionsPlugin;
import org.jboss.tools.common.text.ext.hyperlink.xpl.Messages;
import org.jboss.tools.common.text.ext.util.RegionHolder;
import org.jboss.tools.common.text.ext.util.StructuredModelWrapper;
import org.jboss.tools.common.text.ext.util.StructuredSelectionHelper;
import org.jboss.tools.common.text.ext.util.Utils;
import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.css.CSSRule;
import org.w3c.dom.css.CSSRuleList;
import org.w3c.dom.css.CSSStyleSheet;

/**
 * @author Jeremy
 */
public class CSSClassHyperlink extends AbstractHyperlink {

	public static final String[] STYLE_TAGS = new String[] { "style", "link" }; //$NON-NLS-1$//$NON-NLS-2$
	public static final String LINK_TAG = "link"; //$NON-NLS-1$
	public static final String HREF_ATTRIBUTE = "href"; //$NON-NLS-1$
	public static final String COMPARE_CLASS_REGEX_PREFIX = "[\\.]?"; //$NON-NLS-1$
	public static final String CONTEXT_PATH_EXPRESSION = "^\\s*(\\#|\\$)\\{facesContext.externalContext.requestContextPath\\}"; //$NON-NLS-1$

	/**
	 * 
	 */
	protected void doHyperlink(IRegion region) {

		IDOMModel model = (IDOMModel) getModelManager()
				.getExistingModelForRead(getDocument());

		// get name of looked for style
		String styleName = getStyleName(region);

		// get elements which copntans information about styles (style and link
		// tags)
		List<Node> styleElementList = getStyleContainerList(model);

		// sort nodes by position in inverse order - from larger position to
		// smaller
		Collections.sort(styleElementList, new Comparator<Node>() {

			public int compare(Node o1, Node o2) {
				return ((IDOMNode) o2).getStartOffset()
						- ((IDOMNode) o1).getStartOffset();
			}

		});

		RegionHolder styleRegion = null;
		// look for style in each Style element
		for (Node styleContainer : styleElementList) {

			styleRegion = findStyleRegion(styleContainer, styleName);

			if (styleRegion != null) {
				showRegion(styleRegion);
				break;
			}

		}

		model.releaseFromRead();
	}

	/**
	 * 
	 * @param model
	 * @return
	 */
	private List<Node> getStyleContainerList(IDOMModel model) {

		// get model of current page
		IDOMDocument document = model.getDocument();

		List<Node> getStyleContainerList = new ArrayList<Node>();

		// get all tags which contains style ( link, style)
		for (String tagName : STYLE_TAGS) {
			getStyleContainerList.addAll(getList(document
					.getElementsByTagName(tagName)));
		}

		return getStyleContainerList;
	}

	/**
	 * move nodes from NodeList to List
	 * 
	 * @param nodeList
	 * @return
	 */
	List<Node> getList(NodeList nodeList) {

		List<Node> newContainerList = new ArrayList<Node>();
		for (int i = 0; i < nodeList.getLength(); i++) {
			newContainerList.add(nodeList.item(i));
		}
		return newContainerList;
	}

	/**
	 * 
	 * @param stylesContainer
	 * @param styleName
	 * @return
	 */
	public RegionHolder findStyleRegion(Node stylesContainer, String styleName) {

		// get style sheet
		CSSStyleSheet sheet = getSheet(stylesContainer);

		if (sheet != null) {

			CSSRuleList ruleList = sheet.getCssRules();

			// for each cssRule
			for (int i = 0; i < ruleList.getLength(); i++) {

				CSSRule cssRule = ruleList.item(i);

				// if cssRule describe looked for style
				if (isRuleMatch(cssRule, styleName)) {

					return new RegionHolder(getFile((ICSSNode) cssRule),
							getRegion(cssRule));

				}
			}

		}
		return null;
	}

	/**
	 * 
	 * @param stylesContainer
	 * @return
	 */
	private CSSStyleSheet getSheet(final Node stylesContainer) {

		INodeNotifier notifier = (INodeNotifier) stylesContainer;

		IStyleSheetAdapter adapter = (IStyleSheetAdapter) notifier
				.getAdapterFor(IStyleSheetAdapter.class);

		if (LINK_TAG.equalsIgnoreCase(stylesContainer.getNodeName())
				&& !(adapter instanceof ExtendedLinkElementAdapter)) {

			notifier.removeAdapter(adapter);
			adapter = new ExtendedLinkElementAdapter(
					(Element) stylesContainer);
			notifier.addAdapter(adapter);

		}

		CSSStyleSheet sheet = null;

		if (adapter != null) {
			sheet = (CSSStyleSheet) adapter.getSheet();

		}

		return sheet;

	}

	protected String processURL(String href) {
		// TODO Auto-generated method stub
		return null;
	}

	/**
	 * 
	 * @param cssRule
	 * @param styleName
	 * @return
	 */
	private boolean isRuleMatch(CSSRule cssRule, String styleName) {

		// get selector text
		String selectorText = ((ICSSStyleRule) cssRule).getSelectorText();

		if (selectorText != null) {

			// split selector text by whitespace
			String[] styles = selectorText.trim().split(" "); //$NON-NLS-1$
			int searchIndex = Arrays.binarySearch(styles, styleName,
					new Comparator<String>() {

						public int compare(String o1, String o2) {
							Matcher matcher = Pattern.compile(
									COMPARE_CLASS_REGEX_PREFIX + o2)
									.matcher(o1);
							return matcher.matches() ? 0 : 1;
						}

					});
			if (searchIndex >= 0)
				return true;
		}
		return false;
	}

	/**
	 * 
	 * @param node
	 * @return
	 */
	private IFile getFile(ICSSNode node) {

		ICSSDocument sheet = node.getOwnerDocument();

		return AbstractHyperlink.getFile(sheet.getModel());
	}

	/**
	 * 
	 * @param cssRule
	 * @return
	 */
	protected Region getRegion(CSSRule cssRule) {

		int offset = ((IndexedRegion) cssRule).getStartOffset();

		// if css rule is contained in style tag so it is require to take into
		// account offset of this tag
		ICSSStyleSheet document = (ICSSStyleSheet) ((ICSSStyleRule) cssRule)
				.getOwnerDocument();

		ICSSModel model = document.getModel();

		// get style tag
		Node node = model.getOwnerDOMNode();

		// increase offset
		if (node instanceof IDOMElement)
			offset += ((IDOMElement) node).getStartEndOffset();

		return new Region(offset, 0);

	}

	/**
	 * 
	 * @param styleRegion
	 */
	private void showRegion(RegionHolder styleRegion) {

		IWorkbenchPage workbenchPage = ExtensionsPlugin.getDefault()
				.getWorkbench().getActiveWorkbenchWindow().getActivePage();
		IEditorPart part = null;
		if (styleRegion.file != null) {
			try {
				part = IDE.openEditor(workbenchPage, styleRegion.file, true);
			} catch (PartInitException e) {
				e.printStackTrace();
			}
			if (part == null) {
				openFileFailed();
				return;
			}
		}
		StructuredSelectionHelper.setSelectionAndReveal(part,
				styleRegion.region);
	}

	/**
	 * 
	 * @param region
	 * @return
	 */
	private String getStyleName(IRegion region) {
		try {
			return getDocument().get(region.getOffset(), region.getLength());
		} catch (BadLocationException e) {
			return null;
		}
	}

	IRegion fLastRegion = null;

	/**
	 * @see com.ibm.sse.editor.AbstractHyperlink#doGetHyperlinkRegion(int)
	 */
	protected IRegion doGetHyperlinkRegion(int offset) {
		fLastRegion = getRegion(offset);
		return fLastRegion;
	}

	/**
	 * TODO research method
	 * 
	 * @param offset
	 * @return
	 */
	protected IRegion getRegion(int offset) {
		StructuredModelWrapper smw = new StructuredModelWrapper();
		try {
			smw.init(getDocument());
			Document xmlDocument = smw.getDocument();
			if (xmlDocument == null)
				return null;

			Node n = Utils.findNodeForOffset(xmlDocument, offset);

			if (n == null || !(n instanceof Attr))
				return null;
			int start = Utils.getValueStart(n);
			int end = Utils.getValueEnd(n);
			if (start > offset)
				return null;

			String attrText = getDocument().get(start, end - start);

			StringBuffer sb = new StringBuffer(attrText);
			// find start of bean property
			int bStart = offset - start;
			while (bStart >= 0) {
				if (!Character.isJavaIdentifierPart(sb.charAt(bStart))
						&& sb.charAt(bStart) != '_' && sb.charAt(bStart) != '-'
						&& sb.charAt(bStart) != '.') {
					bStart++;
					break;
				}

				if (bStart == 0)
					break;
				bStart--;
			}
			// find end of bean property
			int bEnd = offset - start;
			while (bEnd < sb.length()) {
				if (!Character.isJavaIdentifierPart(sb.charAt(bEnd))
						&& sb.charAt(bEnd) != '_' && sb.charAt(bEnd) != '-'
						&& sb.charAt(bEnd) != '.')
					break;
				bEnd++;
			}

			final int propStart = bStart + start;
			final int propLength = bEnd - bStart;

			if (propStart > offset || propStart + propLength < offset)
				return null;
			return new Region(propStart, propLength);
		} catch (BadLocationException x) {
			// ignore
			return null;
		} finally {
			smw.dispose();
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see IHyperlink#getHyperlinkText()
	 */
	public String getHyperlinkText() {
		String styleName = getStyleName(fLastRegion);
		if (styleName == null)
			return MessageFormat.format(Messages.OpenA, Messages.CSSStyle);

		return MessageFormat.format(Messages.OpenCSSStyle, styleName);
	}

	@Override
	protected String findAndReplaceElVariable(String fileName) {
		if (fileName != null)
			fileName = fileName
					.replaceFirst(
							"^\\s*(\\#|\\$)\\{facesContext.externalContext.requestContextPath\\}",
							"");
		return super.findAndReplaceElVariable(fileName);
	}

	public class ExtendedLinkElementAdapter extends LinkElementAdapter {

		private Element element;

		public ExtendedLinkElementAdapter(Element element) {
			this.element = element;
		}

		@Override
		public Element getElement() {
			return element;
		}

		@Override
		protected boolean isValidAttribute() {
			String href = getElement().getAttribute(HREF_ATTRIBUTE);
			if (href == null || href.length() == 0)
				return false;
			return true;
		}

		/**
		 */
		public ICSSModel getModel() {
			ICSSModel model = super.getModel();
			if (model == null) {
				model = retrieveModel();
				setModel(model);
			}
			return model;
		}

		/**
		 */
		private ICSSModel retrieveModel() {
			if (!isValidAttribute()) {
				return null;
			}

			// null,attr check is done in isValidAttribute()
			Element element = getElement();
			String href = findAndReplaceElVariable(element
					.getAttribute(HREF_ATTRIBUTE));

			IDOMModel baseModel = ((IDOMNode) element).getModel();
			if (baseModel == null)
				return null;
			Object id = baseModel.getId();
			if (!(id instanceof String))
				return null;
			// String base = (String)id;

			// get ModelProvideAdapter
			IModelProvideAdapter adapter = (IModelProvideAdapter) ((INodeNotifier) getElement())
					.getAdapterFor(IModelProvideAdapter.class);

			URLModelProvider provider = new URLModelProvider();
			try {
				IStructuredModel newModel = provider.getModelForRead(baseModel,
						href);
				if (newModel == null)
					return null;
				if (!(newModel instanceof ICSSModel)) {
					newModel.releaseFromRead();
					return null;
				}

				// notify adapter
				if (adapter != null)
					adapter.modelProvided(newModel);

				return (ICSSModel) newModel;
			} catch (UnsupportedEncodingException e) {
			} catch (IOException e) {
			}

			return null;
		}
	}

}