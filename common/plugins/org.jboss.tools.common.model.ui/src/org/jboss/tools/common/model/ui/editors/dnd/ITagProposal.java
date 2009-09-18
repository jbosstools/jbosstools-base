package org.jboss.tools.common.model.ui.editors.dnd;

public interface ITagProposal {
	public static final IAttributeValueLoader EMPTY_ATTRIBUTE_VALUE_LOADER = new IAttributeValueLoader() {
		public void fillTagAttributes(IDropWizardModel model) {
			// do nothing
		}
	};
	public static String EMPTY_PREFIX = "";

	public String getName();

	public String getPrefix();

	public IAttributeValueLoader getAttributesValueLoader();

	public String getDisplayString();

	public String getDetails();

}
