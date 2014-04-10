package org.jboss.tools.common.model.ui.internal.editors;

public class PaletteItemResult {
	protected String startText;
	protected String endText;
	
	public PaletteItemResult(String startText, String endText) {
		this.startText = startText;
		this.endText = endText;
	}

	public String getStartText() {
		return startText;
	}

	public String getEndText() {
		return endText;
	}
}
