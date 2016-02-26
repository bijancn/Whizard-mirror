	/*
	 * [Appearance]
	 * Selecting Tabs:Integration > Process
	 */
	 $(document).on("click", ".process-entry", function() {
		$('.process-entry').removeClass('active');
		$(this).addClass('active');

	});
	
