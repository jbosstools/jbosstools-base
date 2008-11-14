/*******************************************************************************
 * Copyright (c) 2007 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.model.ui.attribute;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.bindings.keys.KeyStroke;
import org.eclipse.jface.bindings.keys.ParseException;
import org.eclipse.jface.fieldassist.ComboContentAdapter;
import org.eclipse.jface.fieldassist.ContentProposalAdapter;
import org.eclipse.jface.fieldassist.IContentProposal;
import org.eclipse.jface.fieldassist.IContentProposalProvider;
import org.eclipse.jface.fieldassist.IControlContentAdapter;
import org.eclipse.jface.fieldassist.TextContentAdapter;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Text;
import org.jboss.tools.common.meta.XAttribute;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.model.ui.attribute.adapter.DefaultValueAdapter;

/**
 * @author Viacheslav Kabanovich
 */
public class AttributeContentProposalProviderFactory {
	private static List<IAttributeContentProposalProvider> EMPTY = new ArrayList<IAttributeContentProposalProvider>();

	public static KeyStroke getCtrlSpaceKeyStroke() {
		KeyStroke ks = null;
		
		try {
			ks = KeyStroke.getInstance("Ctrl+Space");
		} catch (ParseException e) {
			//Cannot happen, this code is safe.
			ModelUIPlugin.getPluginLog().logError(e);
		}
		
		return ks;
	}

	public static void registerContentAssist(DefaultValueAdapter valueAdapter, Control control) {
		IControlContentAdapter controlAdapter = control instanceof Text 
			? new TextContentAdapter()
			: control instanceof Combo
			? new ComboContentAdapter()
			: null;
		if(controlAdapter == null) {
			return;
		}
		XModelObject object = valueAdapter.getModelObject();
		XAttribute attr = valueAdapter.getAttribute();
		if (attr == null && valueAdapter.getAttributeData() != null) {
			attr = valueAdapter.getAttributeData().getAttribute();
		}
		AttributeContentProposalProviderFactory factory = new AttributeContentProposalProviderFactory();
		final List<IAttributeContentProposalProvider> ps = factory
				.getContentProposalProviders(object, attr);
		for (IAttributeContentProposalProvider p : ps) {
			p.init(object, attr);
			IContentProposalProvider cpp = p.getContentProposalProvider();
			if (cpp == null)
				continue;
			ContentProposalAdapter adapter = new ContentProposalAdapter(
				control, 
				controlAdapter, 
				cpp,
				AttributeContentProposalProviderFactory.getCtrlSpaceKeyStroke(), 
				null);
			adapter.setPropagateKeys(true);
			adapter.setProposalAcceptanceStyle(p.getProposalAcceptanceStyle());
		}
		if (!ps.isEmpty()) {
			control.addDisposeListener(new DisposeListener() {
				public void widgetDisposed(DisposeEvent e) {
					for (IAttributeContentProposalProvider p : ps) {
						p.dispose();
					}
				}
			});
		}
	}

	static String POINT_ID = ModelUIPlugin.PLUGIN_ID + ".attributeContentProposalProviders";

	public List<IAttributeContentProposalProvider> getContentProposalProviders(XModelObject object, XAttribute attribute) {
		List<IAttributeContentProposalProvider> result = EMPTY;

		IExtensionPoint point = Platform.getExtensionRegistry().getExtensionPoint(POINT_ID);
		
		IExtension[] es = point.getExtensions();
		for (int i = 0; i < es.length; i++) {
			IConfigurationElement[] cs = es[i].getConfigurationElements();
			for (int j = 0; j < cs.length; j++) {
				IAttributeContentProposalProvider provider = null;
				try {
					Object o = cs[j].createExecutableExtension("class");
					provider = (IAttributeContentProposalProvider)o;
				} catch (CoreException e) {
					ModelUIPlugin.getPluginLog().logError(e);
				}
				if(provider.isRelevant(object, attribute)) {
					if(result == EMPTY) result = new ArrayList<IAttributeContentProposalProvider>();
					result.add(provider);
				}
			}
		}

		if(false) {
			//Test
			result.add(new TestAttributeContentProposalProvider());
		}
		
		return result;
	}

	public static IContentProposal makeContentProposal(final String proposal, final String label) {
		return new IContentProposal() {
			public String getContent() {
				return proposal;
			}

			public String getDescription() {
				return null;
			}

			public String getLabel() {
				return label;
			}

			public int getCursorPosition() {
				return proposal.length();
			}
		};
	}

}

/**
 * Example
 * @author glory
 *
 */
class TestAttributeContentProposalProvider implements IAttributeContentProposalProvider {

	public boolean isRelevant(XModelObject object, XAttribute attribute) {
		return true;
	}

	public void init(XModelObject object, XAttribute attribute) {
	}

	public IContentProposalProvider getContentProposalProvider() {
		IContentProposalProvider cpp = new IContentProposalProvider() {

			public IContentProposal[] getProposals(String contents,
					int position) {
				ArrayList<IContentProposal> ps = new ArrayList<IContentProposal>();
				
				if(position <= contents.length() && position > 3 && "test".equals(contents.substring(position - 4, position))) {
					ps.add(AttributeContentProposalProviderFactory.makeContentProposal("aaa", ".aaa"));
				}
				
				return ps.toArray(new IContentProposal[0]);
			}
			
		};
		return cpp;
	}

	public int getProposalAcceptanceStyle() {
		return ContentProposalAdapter.PROPOSAL_INSERT;
	}

	public void dispose() {
	}

}