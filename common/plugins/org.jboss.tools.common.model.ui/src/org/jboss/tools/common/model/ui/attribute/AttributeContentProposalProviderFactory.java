package org.jboss.tools.common.model.ui.attribute;

import java.util.ArrayList;
import java.util.List;

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
					System.out.println("dispose");
					for (IAttributeContentProposalProvider p : ps) {
						p.dispose();
					}
				}
			});
		}
	}

	public List<IAttributeContentProposalProvider> getContentProposalProviders(XModelObject object, XAttribute attribute) {
		if(true) {
			List<IAttributeContentProposalProvider> list = new ArrayList<IAttributeContentProposalProvider>();
			list.add(new TestAttributeContentProposalProvider());
			return list;
		}
		
		return EMPTY;
	}
}

class TestAttributeContentProposalProvider implements IAttributeContentProposalProvider {

	public void dispose() {
	}

	public IContentProposalProvider getContentProposalProvider() {
		IContentProposalProvider cpp = new IContentProposalProvider() {

			public IContentProposal[] getProposals(String contents,
					int position) {
				ArrayList<IContentProposal> ps = new ArrayList<IContentProposal>();
				
				if(position <= contents.length() && position > 0 && 'b' == contents.charAt(position - 1))
				ps.add(makeContentProposal("aaa", ".aaa"));
				
				return ps.toArray(new IContentProposal[0]);
			}
			
		};
		return cpp;
	}

	public int getProposalAcceptanceStyle() {
		return ContentProposalAdapter.PROPOSAL_INSERT;
	}

	public void init(XModelObject object, XAttribute attribute) {
	}

	private IContentProposal makeContentProposal(final String proposal, final String label) {
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