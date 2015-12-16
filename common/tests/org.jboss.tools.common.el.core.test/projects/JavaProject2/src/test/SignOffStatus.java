package test;

public enum SignOffStatus {
	VALID("Valid"),
	ALMOST_EXPIRED("Almost expired"),
	EXPIRED("Expired");
 
	private final String label;

	private SignOffStatus(String label) {
		this.label = label;
	}
 
	public String getLabel() {
		return label;
	}
}
